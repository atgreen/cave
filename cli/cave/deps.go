package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"net/url"
	"os"
	"strings"
	"text/tabwriter"
)

// Dep is a resolved dependency-graph row.
type Dep struct {
	ID           int64  `json:"id"`
	Ecosystem    string `json:"ecosystem"`
	PackageName  string `json:"package_name"`
	Version      string `json:"version"`
	Purl         string `json:"purl"`
	IsDirect     bool   `json:"is_direct"`
	ManifestPath string `json:"manifest_path"`
}

// Alert is a security alert joined with its dep + advisory.
type Alert struct {
	ID          int64  `json:"id"`
	State       string `json:"state"`
	Ecosystem   string `json:"ecosystem"`
	PackageName string `json:"package_name"`
	Version     string `json:"version"`
	OsvID       string `json:"osv_id"`
	Severity    string `json:"severity"`
	Summary     string `json:"summary"`
	FixVersion  string `json:"fix_version"`
}

// Usage is one repo using a given package (org-wide query).
type Usage struct {
	RepoID   int64  `json:"repo_id"`
	Ref      string `json:"ref"`
	Version  string `json:"version"`
	IsDirect bool   `json:"is_direct"`
}

type dismissRequest struct {
	Reason string `json:"reason"`
	Note   string `json:"note,omitempty"`
}

func depsURL(baseURL, ownerName, repoName, suffix string, query url.Values) string {
	path := fmt.Sprintf("%s/api/v1/repos/%s/%s/%s", baseURL,
		url.PathEscape(ownerName), url.PathEscape(repoName), suffix)
	if len(query) > 0 {
		path += "?" + query.Encode()
	}
	return path
}

func (c *client) listDeps(ownerName, repoName, ref string) ([]Dep, error) {
	q := url.Values{}
	if ref != "" {
		q.Set("ref", ref)
	}
	var deps []Dep
	err := c.doJSON("GET", depsURL(c.baseURL, ownerName, repoName, "deps", q), nil, &deps)
	return deps, err
}

func (c *client) listAlerts(ownerName, repoName, state string) ([]Alert, error) {
	q := url.Values{}
	if state != "" {
		q.Set("state", state)
	}
	var alerts []Alert
	err := c.doJSON("GET", depsURL(c.baseURL, ownerName, repoName, "alerts", q), nil, &alerts)
	return alerts, err
}

func (c *client) dismissAlert(ownerName, repoName, id string, payload dismissRequest) (*Alert, error) {
	var alert Alert
	suffix := fmt.Sprintf("alerts/%s/dismiss", url.PathEscape(id))
	if err := c.doJSON("POST", depsURL(c.baseURL, ownerName, repoName, suffix, nil), payload, &alert); err != nil {
		return nil, err
	}
	return &alert, nil
}

func (c *client) depsUsage(ecosystem, pkg string) ([]Usage, error) {
	q := url.Values{}
	q.Set("ecosystem", ecosystem)
	q.Set("package", pkg)
	var rows []Usage
	err := c.doJSON("GET", fmt.Sprintf("%s/api/v1/deps/usage?%s", c.baseURL, q.Encode()), nil, &rows)
	return rows, err
}

func runDeps(c *client, ownerName, repoName string, args []string) error {
	if len(args) == 0 {
		printDepsUsage(os.Stderr)
		return errors.New("missing deps command")
	}
	switch args[0] {
	case "list":
		return runDepsList(c, ownerName, repoName, args[1:])
	case "alerts":
		return runDepsAlerts(c, ownerName, repoName, args[1:])
	case "dismiss":
		return runDepsDismiss(c, ownerName, repoName, args[1:])
	case "who-uses":
		return runDepsWhoUses(c, args[1:])
	case "help", "-h", "--help":
		printDepsUsage(os.Stdout)
		return nil
	default:
		printDepsUsage(os.Stderr)
		return fmt.Errorf("unknown deps command %q", args[0])
	}
}

func runDepsList(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("deps list", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	ref := fs.String("ref", "", "Branch/ref to list")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	deps, err := c.listDeps(ownerName, repoName, *ref)
	if err != nil {
		return err
	}
	if *jsonOut {
		return writeJSON(os.Stdout, deps)
	}
	w := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(w, "ECOSYSTEM\tPACKAGE\tVERSION\tDIRECT")
	for _, d := range deps {
		fmt.Fprintf(w, "%s\t%s\t%s\t%t\n", d.Ecosystem, d.PackageName, d.Version, d.IsDirect)
	}
	return w.Flush()
}

func runDepsAlerts(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("deps alerts", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	state := fs.String("state", "open", "Alert state: open, dismissed, fixed")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	alerts, err := c.listAlerts(ownerName, repoName, *state)
	if err != nil {
		return err
	}
	if *jsonOut {
		return writeJSON(os.Stdout, alerts)
	}
	w := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(w, "ID\tSEVERITY\tPACKAGE\tVERSION\tADVISORY\tFIX")
	for _, a := range alerts {
		fmt.Fprintf(w, "%d\t%s\t%s\t%s\t%s\t%s\n",
			a.ID, a.Severity, a.PackageName, a.Version, a.OsvID, a.FixVersion)
	}
	return w.Flush()
}

func runDepsDismiss(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("deps dismiss", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	reason := fs.String("reason", "risk_accepted", "Reason: not_used, no_fix, risk_accepted")
	note := fs.String("note", "", "Optional note")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	if fs.NArg() != 1 {
		return errors.New("usage: cave deps dismiss [flags] <alert-id>")
	}
	alert, err := c.dismissAlert(ownerName, repoName, fs.Arg(0),
		dismissRequest{Reason: strings.TrimSpace(*reason), Note: *note})
	if err != nil {
		return err
	}
	if *jsonOut {
		return writeJSON(os.Stdout, alert)
	}
	fmt.Fprintf(os.Stdout, "Dismissed alert #%d (%s) as %s.\n", alert.ID, alert.OsvID, alert.State)
	return nil
}

func runDepsWhoUses(c *client, args []string) error {
	fs := flag.NewFlagSet("deps who-uses", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	ecosystem := fs.String("ecosystem", "", "OSV ecosystem (e.g. npm, Go, PyPI)")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	if fs.NArg() != 1 || strings.TrimSpace(*ecosystem) == "" {
		return errors.New("usage: cave deps who-uses --ecosystem <eco> <package>")
	}
	rows, err := c.depsUsage(strings.TrimSpace(*ecosystem), fs.Arg(0))
	if err != nil {
		return err
	}
	if *jsonOut {
		return writeJSON(os.Stdout, rows)
	}
	w := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(w, "REPO_ID\tREF\tVERSION\tDIRECT")
	for _, r := range rows {
		fmt.Fprintf(w, "%d\t%s\t%s\t%t\n", r.RepoID, r.Ref, r.Version, r.IsDirect)
	}
	return w.Flush()
}

func printDepsUsage(w io.Writer) {
	fmt.Fprintln(w, "Dependency commands:")
	fmt.Fprintln(w, "  deps list [--ref R] [--json]                 List the dependency graph")
	fmt.Fprintln(w, "  deps alerts [--state S] [--json]             List security alerts")
	fmt.Fprintln(w, "  deps dismiss [--reason R] [--note N] <id>    Dismiss an alert")
	fmt.Fprintln(w, "  deps who-uses --ecosystem E <package>        Repos using a package (org-wide)")
}
