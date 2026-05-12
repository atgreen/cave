package backup

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/runtime"
)

const maxLocalBackups = 10

// Backup creates a backup archive of a Cave instance.
func Backup(cfg *config.Config, rt runtime.Runtime, backupDir string) (string, error) {
	prefix := cfg.Runtime.Prefix
	timestamp := time.Now().Format("2006-01-02-150405")

	if backupDir == "" {
		home, _ := os.UserHomeDir()
		backupDir = filepath.Join(home, "cave-backups")
	}
	if err := os.MkdirAll(backupDir, 0755); err != nil {
		return "", fmt.Errorf("creating backup dir: %w", err)
	}

	// Create temp working directory
	workDir, err := os.MkdirTemp("", "cave-backup-")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(workDir)

	snapshotName := fmt.Sprintf("%s-%s", prefix, timestamp)
	snapshot := filepath.Join(workDir, snapshotName)
	if err := os.MkdirAll(snapshot, 0755); err != nil {
		return "", err
	}

	pgName := cfg.ContainerName("pg")
	caveName := cfg.ContainerName("cave")

	// 1. Dump cave database
	fmt.Println("  Dumping cave database...")
	if err := rt.ExecToFile(pgName,
		[]string{"pg_dump", "-U", "cave", "-d", "cave", "--format=custom"},
		filepath.Join(snapshot, "cave.pgdump")); err != nil {
		return "", fmt.Errorf("pg_dump cave: %w", err)
	}

	// 2. Dump keycloak database (optional)
	if cfg.KeycloakEnabled() {
		fmt.Println("  Dumping keycloak database...")
		err := rt.ExecToFile(pgName,
			[]string{"pg_dump", "-U", "cave", "-d", "keycloak", "--format=custom"},
			filepath.Join(snapshot, "keycloak.pgdump"))
		if err != nil {
			fmt.Println("    (no keycloak DB, skipping)")
		}
	}

	// 3. Copy git repos
	fmt.Println("  Backing up git repositories...")
	err = rt.Copy(caveName+":/var/lib/cave/repos", filepath.Join(snapshot, "repos"))
	if err != nil {
		fmt.Println("    (no repos directory)")
	}

	// 4. Copy config
	fmt.Println("  Backing up config...")
	rt.Copy(caveName+":/etc/cave.conf", filepath.Join(snapshot, "cave.conf"))

	// 5. Copy cave.yaml if it exists next to where we're running
	for _, yamlName := range []string{prefix + ".yaml", "cave.yaml"} {
		if _, err := os.Stat(yamlName); err == nil {
			data, _ := os.ReadFile(yamlName)
			os.WriteFile(filepath.Join(snapshot, "cave.yaml"), data, 0644)
			break
		}
	}

	// 6. Copy SSH authorized_keys
	rt.Copy(caveName+":/home/cave/.ssh/authorized_keys",
		filepath.Join(snapshot, "authorized_keys"))

	// 7. Metadata
	caveVersion, _ := rt.Exec(caveName, []string{"cave", "--version"})
	pgVersion, _ := rt.Exec(pgName, []string{"postgres", "--version"})
	schemaVersion, _ := rt.Exec(pgName, []string{"psql", "-U", "cave", "-d", "cave", "-Atc",
		"SELECT COALESCE(MAX(version),0) FROM cave_schema_version"})

	repoCount := 0
	reposDir := filepath.Join(snapshot, "repos")
	if entries, err := os.ReadDir(reposDir); err == nil {
		for _, e := range entries {
			if e.IsDir() {
				subs, _ := os.ReadDir(filepath.Join(reposDir, e.Name()))
				for _, s := range subs {
					if s.IsDir() && strings.HasSuffix(s.Name(), ".git") {
						repoCount++
					}
				}
			}
		}
	}

	metadata := fmt.Sprintf(`instance: %s
timestamp: %s
cave_version: %s
postgres_version: %s
schema_version: %s
repo_count: %d
`, prefix, timestamp,
		strings.TrimSpace(caveVersion),
		strings.TrimSpace(pgVersion),
		strings.TrimSpace(schemaVersion),
		repoCount)

	os.WriteFile(filepath.Join(snapshot, "metadata.txt"), []byte(metadata), 0644)
	fmt.Print("\n" + metadata)

	// 8. Compress
	archiveName := fmt.Sprintf("%s-%s.tar.gz", prefix, timestamp)
	archivePath := filepath.Join(backupDir, archiveName)
	cmd := exec.Command("tar", "-czf", archivePath, "-C", workDir, snapshotName)
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("creating archive: %w", err)
	}

	// 9. Prune old backups
	pruneBackups(backupDir, prefix)

	return archivePath, nil
}

// Restore restores a Cave instance from a backup archive.
func Restore(cfg *config.Config, rt runtime.Runtime, archivePath string) error {
	prefix := cfg.Runtime.Prefix
	pgName := cfg.ContainerName("pg")
	caveName := cfg.ContainerName("cave")

	if _, err := os.Stat(archivePath); err != nil {
		return fmt.Errorf("archive not found: %s", archivePath)
	}

	// Extract
	workDir, err := os.MkdirTemp("", "cave-restore-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(workDir)

	cmd := exec.Command("tar", "-xzf", archivePath, "-C", workDir)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("extracting archive: %w", err)
	}

	// Find snapshot directory
	entries, _ := os.ReadDir(workDir)
	if len(entries) == 0 {
		return fmt.Errorf("archive is empty")
	}
	snapshot := filepath.Join(workDir, entries[0].Name())

	// Show metadata
	if data, err := os.ReadFile(filepath.Join(snapshot, "metadata.txt")); err == nil {
		fmt.Println("Backup metadata:")
		fmt.Println(string(data))
	}

	// Verify containers are running
	pgInfo, err := rt.Inspect(pgName)
	if err != nil || pgInfo.Status != "running" {
		return fmt.Errorf("postgres container %q is not running — start the instance first", pgName)
	}
	caveInfo, err := rt.Inspect(caveName)
	if err != nil || caveInfo.Status != "running" {
		return fmt.Errorf("cave container %q is not running — start the instance first", caveName)
	}

	// Check labels
	if caveInfo.Labels != nil && caveInfo.Labels["cave.managed-by"] != "cavectl" {
		return fmt.Errorf("container %q was not created by cavectl — refusing to restore", caveName)
	}

	// 1. Restore cave database
	fmt.Println("  Restoring cave database...")
	rt.Exec(pgName, []string{"dropdb", "-U", "cave", "--if-exists", "cave"})
	if _, err := rt.Exec(pgName, []string{"createdb", "-U", "cave", "cave"}); err != nil {
		return fmt.Errorf("createdb cave: %w", err)
	}
	caveDump := filepath.Join(snapshot, "cave.pgdump")
	if _, err := os.Stat(caveDump); err == nil {
		if err := rt.ExecFromFile(pgName,
			[]string{"pg_restore", "-U", "cave", "-d", "cave", "--no-owner"},
			caveDump); err != nil {
			return fmt.Errorf("pg_restore cave: %w", err)
		}
	}

	// 2. Restore keycloak database (if present)
	kcDump := filepath.Join(snapshot, "keycloak.pgdump")
	if _, err := os.Stat(kcDump); err == nil {
		fmt.Println("  Restoring keycloak database...")
		rt.Exec(pgName, []string{"dropdb", "-U", "cave", "--if-exists", "keycloak"})
		rt.Exec(pgName, []string{"createdb", "-U", "cave", "keycloak"})
		rt.ExecFromFile(pgName,
			[]string{"pg_restore", "-U", "cave", "-d", "keycloak", "--no-owner"},
			kcDump)
	}

	// 3. Restore git repos
	reposDir := filepath.Join(snapshot, "repos")
	if _, err := os.Stat(reposDir); err == nil {
		fmt.Println("  Restoring git repositories...")
		rt.Exec(caveName, []string{"rm", "-rf", "/var/lib/cave/repos"})
		if err := rt.Copy(reposDir, caveName+":/var/lib/cave/repos"); err != nil {
			return fmt.Errorf("copying repos: %w", err)
		}
		rt.Exec(caveName, []string{"chown", "-R", "cave:cave", "/var/lib/cave/repos"})
	}

	// 4. Regenerate authorized_keys
	fmt.Println("  Regenerating authorized_keys...")
	rt.Exec(caveName, []string{"cave", "update-keys",
		"--config", "/etc/cave.conf",
		"--output", "/home/cave/.ssh/authorized_keys",
		"--cave-shell", "/usr/bin/cave-shell.sh"})

	fmt.Printf("\nRestore complete. Restart the instance to pick up all changes:\n")
	fmt.Printf("  %s stop %s && %s start %s\n", rt.Name(), prefix, rt.Name(), prefix)

	_ = prefix // used in message
	return nil
}

func pruneBackups(dir, prefix string) {
	pattern := filepath.Join(dir, prefix+"-*.tar.gz")
	matches, _ := filepath.Glob(pattern)
	if len(matches) <= maxLocalBackups {
		return
	}
	sort.Strings(matches) // oldest first (timestamp in name)
	for _, f := range matches[:len(matches)-maxLocalBackups] {
		os.Remove(f)
	}
	fmt.Printf("  Pruned old backups, keeping last %d.\n", maxLocalBackups)
}
