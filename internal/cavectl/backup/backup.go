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

	// 2. Copy git repos
	fmt.Println("  Backing up git repositories...")
	err = rt.Copy(caveName+":/var/lib/cave/repos", filepath.Join(snapshot, "repos"))
	if err != nil {
		fmt.Println("    (no repos directory)")
	}

	// 3. Copy config
	fmt.Println("  Backing up config...")
	rt.Copy(caveName+":/etc/cave.conf", filepath.Join(snapshot, "cave.conf"))

	// 4. Copy cave.yaml if it exists next to where we're running
	for _, yamlName := range []string{prefix + ".yaml", "cave.yaml"} {
		if _, err := os.Stat(yamlName); err == nil {
			data, _ := os.ReadFile(yamlName)
			os.WriteFile(filepath.Join(snapshot, "cave.yaml"), data, 0644)
			break
		}
	}

	// 5. Copy SSH authorized_keys
	rt.Copy(caveName+":/home/cave/.ssh/authorized_keys",
		filepath.Join(snapshot, "authorized_keys"))

	// 6. Metadata
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

	// 7. Compress
	archiveName := fmt.Sprintf("%s-%s.tar.gz", prefix, timestamp)
	archivePath := filepath.Join(backupDir, archiveName)
	cmd := exec.Command("tar", "-czf", archivePath, "-C", workDir, snapshotName)
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("creating archive: %w", err)
	}

	// 8. Prune old backups
	pruneBackups(backupDir, prefix)

	return archivePath, nil
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
