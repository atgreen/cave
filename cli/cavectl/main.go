package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"moxielogic.com/cave/internal/cavectl/apply"
	"moxielogic.com/cave/internal/cavectl/backup"
	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/instance"
	"moxielogic.com/cave/internal/cavectl/plan"
	"moxielogic.com/cave/internal/cavectl/quadlet"
	"moxielogic.com/cave/internal/cavectl/runtime"
	"moxielogic.com/cave/internal/cavectl/state"
)

const defaultFile = "cave.yaml"

func main() {
	if len(os.Args) < 2 {
		usage()
		os.Exit(1)
	}

	cmd := os.Args[1]
	args := os.Args[2:]

	var err error
	switch cmd {
	case "init":
		err = cmdInit(args)
	case "apply":
		err = cmdApply(args)
	case "status":
		err = cmdStatus(args)
	case "instances":
		err = cmdInstances(args)
	case "logs":
		err = cmdLogs(args)
	case "destroy":
		err = cmdDestroy(args)
	case "backup":
		err = cmdBackup(args)
	case "restore":
		fmt.Fprintln(os.Stderr, "Use: cavectl init --from-backup <archive.tar.gz>")
		os.Exit(1)
	case "version":
		fmt.Println("cavectl v0.1.0")
	case "help", "--help", "-h":
		usage()
	default:
		fmt.Fprintf(os.Stderr, "unknown command: %s\n", cmd)
		usage()
		os.Exit(1)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

func usage() {
	fmt.Println(`cavectl — declarative Cave deployment tool

Usage: cavectl <command> [options]

Commands:
  init         Initialize and start a new Cave instance
  apply        Reconcile deployment to match cave.yaml
  status       Show current deployment state
  instances    List all Cave instances on this host
  logs         Tail container logs
  destroy      Tear down all containers
  backup       Back up database and repositories
  version      Print version

Options:
  -f, --file        Path to cave.yaml (default: cave.yaml)
  --name            Instance name (default: cave)
  --from-backup     Restore from backup archive during init
  --dry-run         Show plan without executing
  --yes             Skip confirmation prompt`)
}

// --- init ---

func cmdInit(args []string) error {
	name := ""
	dir := "."
	yes := false
	fromBackup := ""
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--name":
			i++
			if i < len(args) {
				name = args[i]
			}
		case "--dir":
			i++
			if i < len(args) {
				dir = args[i]
			}
		case "--from-backup":
			i++
			if i < len(args) {
				fromBackup = args[i]
			}
		case "--yes", "-y":
			yes = true
		}
	}

	// Validate backup archive exists
	if fromBackup != "" {
		if _, err := os.Stat(fromBackup); err != nil {
			return fmt.Errorf("backup archive not found: %s", fromBackup)
		}
	}

	// Detect container runtime
	rt, err := runtime.Detect()
	if err != nil {
		return err
	}

	// Discover existing instances
	existing, _ := instance.Discover("")
	if name == "" {
		name = "cave"
		// If default name is taken, require explicit name
		for _, inst := range existing {
			if inst.Name == name {
				return fmt.Errorf("instance %q already exists on this host.\nUse --name to choose a different name, or run: cavectl instances", name)
			}
		}
	} else {
		for _, inst := range existing {
			if inst.Name == name {
				return fmt.Errorf("instance %q already exists on this host.\nRun: cavectl instances", name)
			}
		}
	}

	// Find free ports, avoiding ports used by existing instances
	usedPorts := make(map[int]bool)
	for _, inst := range existing {
		usedPorts[inst.HTTPPort] = true
		usedPorts[inst.SSHPort] = true
	}
	httpPort := findFreePort(9080, usedPorts)
	usedPorts[httpPort] = true
	sshPort := findFreePort(9222, usedPorts)
	usedPorts[sshPort] = true
	keycloakPort := findFreePort(9180, usedPorts)

	// Generate config with random passwords
	cfg := config.Default()
	cfg.Runtime.Prefix = name
	cfg.Runtime.Network = name + "-net"
	cfg.Ports.HTTP = httpPort
	cfg.Ports.SSH = sshPort
	cfg.Ports.Keycloak = keycloakPort
	cfg.Cave.BaseURL = fmt.Sprintf("http://localhost:%d", httpPort)
	cfg.Cave.SecretKey = instance.RandomSecretKey()
	cfg.Database.Password = instance.RandomPassword(24)

	// Pre-flight: check for existing containers/volumes that aren't ours
	if err := checkForCollisions(cfg, rt); err != nil {
		return err
	}

	// Write cave.yaml
	file := filepath.Join(dir, name+".yaml")
	if name == "cave" {
		file = filepath.Join(dir, "cave.yaml")
	}
	data, err := config.Marshal(cfg)
	if err != nil {
		return err
	}
	header := fmt.Sprintf("# Cave instance: %s\n# Edit this file and run: cavectl apply\n\n", name)
	if err := os.WriteFile(file, []byte(header+string(data)), 0644); err != nil {
		return err
	}

	fmt.Printf("Initializing Cave instance...\n\n")
	fmt.Printf("  Instance:  %s\n", name)
	fmt.Printf("  HTTP:      http://localhost:%d\n", httpPort)
	fmt.Printf("  SSH:       localhost:%d\n", sshPort)
	fmt.Printf("  Auth:      %s\n", cfg.Auth.Mode)
	fmt.Printf("  Database:  %s\n", cfg.Database.Mode)
	fmt.Printf("  Config:    %s\n", file)
	fmt.Printf("  Runtime:   %s\n", rt.Name())
	if fromBackup != "" {
		fmt.Printf("  Backup:    %s\n", fromBackup)
	}
	fmt.Println()

	// Compute plan
	current, err := state.Read(cfg, rt)
	if err != nil {
		return fmt.Errorf("reading state: %w", err)
	}

	actions := plan.Diff(cfg, current, fromBackup)
	plan.PrintPlan(actions)

	if len(actions) == 0 {
		fmt.Println("Nothing to do.")
		return nil
	}

	if !yes {
		if !plan.Confirm() {
			fmt.Println("Aborted. cave.yaml has been written — you can run: cavectl apply")
			return nil
		}
	}

	// Execute
	fmt.Println()
	executor := &apply.Executor{Config: cfg, Runtime: rt}
	if err := executor.Execute(actions); err != nil {
		return err
	}

	// Install quadlets on Linux
	if quadlet.Supported() {
		fmt.Println("\n  Installing systemd quadlet units...")
		if err := quadlet.Install(cfg); err != nil {
			fmt.Fprintf(os.Stderr, "  Warning: quadlet install failed: %v\n", err)
		} else {
			fmt.Printf("  Quadlet units installed to %s\n", quadlet.QuadletDir())
		}
	}

	fmt.Printf("\nDone. Cave is running at http://localhost:%d\n", httpPort)
	return nil
}

func checkForCollisions(cfg *config.Config, rt runtime.Runtime) error {
	// Check containers
	for _, svc := range []string{"cave", "pg", "keycloak", "zoekt-web"} {
		name := cfg.ContainerName(svc)
		info, err := rt.Inspect(name)
		if err != nil {
			continue
		}
		if info.Status != "not-found" {
			if info.Labels == nil || info.Labels["cave.managed-by"] != "cavectl" {
				return fmt.Errorf("container %q already exists and was not created by cavectl — choose a different --name", name)
			}
		}
	}
	// Check volumes
	for _, suffix := range []string{"data", "pgdata", "zoekt"} {
		vname := cfg.VolumeName(suffix)
		exists, err := rt.VolumeExists(vname)
		if err != nil {
			continue
		}
		if exists {
			// Volume exists — check if any container using it is ours
			// For safety, just warn but don't block (volumes might be from a previous cavectl destroy that preserved them)
			fmt.Printf("  Note: volume %q already exists (data will be reused)\n", vname)
		}
	}
	return nil
}

func findFreePort(base int, used map[int]bool) int {
	port := base
	for used[port] || !instance.PortAvailable(port) {
		port++
		if port > base+100 {
			return base
		}
	}
	return port
}

// --- apply ---

func cmdApply(args []string) error {
	file := defaultFile
	dryRun := false
	yes := false
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		case "--dry-run":
			dryRun = true
		case "--yes", "-y":
			yes = true
		default:
			if !strings.HasPrefix(args[i], "-") {
				file = args[i]
			}
		}
	}

	cfg, err := config.Load(file)
	if err != nil {
		return err
	}

	rt, err := runtime.ForEngine(cfg.Runtime.Engine)
	if err != nil {
		return err
	}
	fmt.Printf("Using %s\n\n", rt.Name())

	current, err := state.Read(cfg, rt)
	if err != nil {
		return fmt.Errorf("reading current state: %w", err)
	}

	actions := plan.Diff(cfg, current)
	plan.PrintPlan(actions)

	if len(actions) == 0 {
		return nil
	}

	if dryRun {
		return nil
	}

	if !yes {
		if !plan.Confirm() {
			fmt.Println("Aborted.")
			return nil
		}
	}

	fmt.Println()
	executor := &apply.Executor{Config: cfg, Runtime: rt}
	if err := executor.Execute(actions); err != nil {
		return err
	}

	// Update quadlets if on Linux
	if quadlet.Supported() {
		if err := quadlet.Install(cfg); err != nil {
			fmt.Fprintf(os.Stderr, "Warning: quadlet update failed: %v\n", err)
		}
	}

	fmt.Println("\nDone.")
	return nil
}

// --- status ---

func cmdStatus(args []string) error {
	file := defaultFile
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		default:
			if !strings.HasPrefix(args[i], "-") {
				file = args[i]
			}
		}
	}

	cfg, err := config.Load(file)
	if err != nil {
		return err
	}

	rt, err := runtime.ForEngine(cfg.Runtime.Engine)
	if err != nil {
		return err
	}

	current, err := state.Read(cfg, rt)
	if err != nil {
		return fmt.Errorf("reading state: %w", err)
	}

	if !current.IsDeployed() {
		fmt.Println("No deployment found.")
		return nil
	}

	fmt.Printf("Instance: %s\n\n", cfg.Runtime.Prefix)
	fmt.Printf("%-16s %-12s %s\n", "SERVICE", "STATUS", "IMAGE")
	fmt.Printf("%-16s %-12s %s\n", "-------", "------", "-----")
	for svc, info := range current.Containers {
		if info.Status == "not-found" {
			continue
		}
		image := info.Image
		if len(image) > 45 {
			image = "..." + image[len(image)-42:]
		}
		fmt.Printf("%-16s %-12s %s\n", svc, info.Status, image)
	}

	fmt.Printf("\nURL:     http://localhost:%d\n", cfg.Ports.HTTP)
	fmt.Printf("SSH:     localhost:%d\n", cfg.Ports.SSH)
	fmt.Printf("Auth:    %s\n", cfg.Auth.Mode)
	fmt.Printf("DB:      %s\n", cfg.Database.Mode)
	return nil
}

// --- instances ---

func cmdInstances(args []string) error {
	instances, err := instance.Discover("")
	if err != nil {
		return err
	}

	if len(instances) == 0 {
		fmt.Println("No Cave instances found on this host.")
		return nil
	}

	fmt.Printf("%-16s %-12s %-30s %s\n", "NAME", "STATUS", "URL", "SSH")
	fmt.Printf("%-16s %-12s %-30s %s\n", "----", "------", "---", "---")
	for _, inst := range instances {
		url := inst.URL
		if url == "" {
			url = "-"
		}
		ssh := "-"
		if inst.SSHPort > 0 {
			ssh = fmt.Sprintf("localhost:%d", inst.SSHPort)
		}
		fmt.Printf("%-16s %-12s %-30s %s\n", inst.Name, inst.Status, url, ssh)
	}
	return nil
}

// --- logs ---

func cmdLogs(args []string) error {
	file := defaultFile
	service := "cave"
	follow := false
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--follow":
			follow = true
		case "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		case "--service", "-s":
			i++
			if i < len(args) {
				service = args[i]
			}
		}
	}

	cfg, err := config.Load(file)
	if err != nil {
		return err
	}

	rt, err := runtime.ForEngine(cfg.Runtime.Engine)
	if err != nil {
		return err
	}

	name := cfg.ContainerName(service)
	return rt.Logs(name, follow)
}

// --- destroy ---

func cmdDestroy(args []string) error {
	file := defaultFile
	yes := false
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		case "--yes", "-y":
			yes = true
		}
	}

	cfg, err := config.Load(file)
	if err != nil {
		return err
	}

	if !yes {
		fmt.Printf("This will remove all Cave containers for instance %q.\n", cfg.Runtime.Prefix)
		fmt.Printf("Volumes will NOT be removed (data is preserved).\n\n")
		if !plan.Confirm() {
			fmt.Println("Aborted.")
			return nil
		}
	}

	rt, err := runtime.ForEngine(cfg.Runtime.Engine)
	if err != nil {
		return err
	}

	services := []string{"cave"}
	// Add runners (reverse order for clean shutdown)
	for i := cfg.Runner.Count; i >= 1; i-- {
		if i == 1 {
			services = append(services, "runner")
		} else {
			services = append(services, fmt.Sprintf("runner-%d", i))
		}
	}
	services = append(services, "zoekt-web", "keycloak", "pg")
	for _, svc := range services {
		name := cfg.ContainerName(svc)
		info, _ := rt.Inspect(name)
		if info != nil && info.Status != "not-found" {
			if info.Labels == nil || info.Labels["cave.managed-by"] != "cavectl" {
				fmt.Printf("  Skipping %s (not managed by cavectl)\n", name)
				continue
			}
			fmt.Printf("  Removing %s...\n", name)
			rt.Stop(name)
			rt.Remove(name)
		}
	}

	// Remove quadlets if on Linux
	if quadlet.Supported() {
		quadlet.Uninstall(cfg)
	}

	fmt.Println("\nAll containers removed. Volumes preserved.")
	return nil
}

// --- backup ---

func cmdBackup(args []string) error {
	file := defaultFile
	dir := ""
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		case "--dir":
			i++
			if i < len(args) {
				dir = args[i]
			}
		}
	}

	cfg, err := config.Load(file)
	if err != nil {
		return err
	}

	rt, err := runtime.ForEngine(cfg.Runtime.Engine)
	if err != nil {
		return err
	}

	fmt.Printf("Backing up instance %q...\n\n", cfg.Runtime.Prefix)
	archivePath, err := backup.Backup(cfg, rt, dir)
	if err != nil {
		return err
	}

	// Get file size
	info, _ := os.Stat(archivePath)
	size := "unknown"
	if info != nil {
		mb := float64(info.Size()) / 1024 / 1024
		if mb >= 1 {
			size = fmt.Sprintf("%.1f MB", mb)
		} else {
			size = fmt.Sprintf("%.0f KB", float64(info.Size())/1024)
		}
	}

	fmt.Printf("\nBackup complete: %s (%s)\n", archivePath, size)
	return nil
}

