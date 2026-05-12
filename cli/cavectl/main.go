package main

import (
	"fmt"
	"os"
	"strings"

	"moxielogic.com/cave/internal/cavectl/apply"
	"moxielogic.com/cave/internal/cavectl/config"
	"moxielogic.com/cave/internal/cavectl/plan"
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
	case "logs":
		err = cmdLogs(args)
	case "destroy":
		err = cmdDestroy(args)
	case "backup":
		fmt.Fprintln(os.Stderr, "backup: not yet implemented")
		os.Exit(1)
	case "restore":
		fmt.Fprintln(os.Stderr, "restore: not yet implemented")
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
  init       Generate a cave.yaml with sensible defaults
  apply      Reconcile deployment to match cave.yaml
  status     Show current deployment state
  logs       Tail container logs
  destroy    Tear down all containers
  backup     Back up database and repositories
  restore    Restore from backup
  version    Print version

Options:
  -f, --file    Path to cave.yaml (default: cave.yaml)
  --dry-run     Show plan without executing
  --yes         Skip confirmation prompt`)
}

// --- init ---

func cmdInit(args []string) error {
	file := defaultFile
	force := false
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "-f", "--file":
			i++
			if i < len(args) {
				file = args[i]
			}
		case "--force":
			force = true
		}
	}

	if _, err := os.Stat(file); err == nil && !force {
		return fmt.Errorf("%s already exists (use --force to overwrite)", file)
	}

	cfg := config.Default()
	data, err := config.Marshal(cfg)
	if err != nil {
		return err
	}

	// Add a header comment
	header := "# Cave deployment descriptor\n# Edit this file and run: cavectl apply\n\n"
	if err := os.WriteFile(file, []byte(header+string(data)), 0644); err != nil {
		return err
	}

	fmt.Printf("Created %s\n", file)
	fmt.Println("\nNext steps:")
	fmt.Println("  1. Review and edit cave.yaml")
	fmt.Println("  2. Run: cavectl apply")
	return nil
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

	fmt.Printf("\nNetwork: %s\n", cfg.Runtime.Network)
	fmt.Printf("Auth:    %s\n", cfg.Auth.Mode)
	fmt.Printf("DB:      %s\n", cfg.Database.Mode)
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
		fmt.Printf("This will remove all Cave containers for prefix %q.\n", cfg.Runtime.Prefix)
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

	services := []string{"cave", "zoekt-web", "keycloak", "pg"}
	for _, svc := range services {
		name := cfg.ContainerName(svc)
		info, _ := rt.Inspect(name)
		if info != nil && info.Status != "not-found" {
			fmt.Printf("  Removing %s...\n", name)
			rt.Stop(name)
			rt.Remove(name)
		}
	}

	fmt.Println("\nAll containers removed. Volumes preserved.")
	fmt.Println("To also remove volumes, use your container runtime directly.")
	return nil
}
