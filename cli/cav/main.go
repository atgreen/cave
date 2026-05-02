package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	"text/tabwriter"
)

const defaultBaseURL = "http://localhost:8080"

type Issue struct {
	ID        int64  `json:"id"`
	RepoID    int64  `json:"repo-id"`
	Number    int64  `json:"number"`
	AuthorID  int64  `json:"author-id"`
	Title     string `json:"title"`
	Body      string `json:"body"`
	Status    string `json:"status"`
	CreatedAt string `json:"created-at"`
	UpdatedAt string `json:"updated-at"`
	ClosedAt  string `json:"closed-at"`
}

type issueCreateRequest struct {
	Title string `json:"title"`
	Body  string `json:"body,omitempty"`
}

type apiError struct {
	Error string `json:"error"`
}

type client struct {
	baseURL    string
	token      string
	httpClient *http.Client
}

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintf(os.Stderr, "cav: %v\n", err)
		os.Exit(1)
	}
}

func run(args []string) error {
	if len(args) == 0 {
		printUsage(os.Stderr)
		return errors.New("missing command")
	}

	global := flag.NewFlagSet("cav", flag.ContinueOnError)
	global.SetOutput(io.Discard)
	baseURL := global.String("base-url", envOrDefault("CAVE_BASE_URL", defaultBaseURL), "Cave base URL")
	token := global.String("token", os.Getenv("CAVE_TOKEN"), "Cave API token")
	repo := global.String("repo", os.Getenv("CAVE_REPO"), "Repository in OWNER/REPO form")
	owner := global.String("owner", os.Getenv("CAVE_OWNER"), "Repository owner")
	repoName := global.String("name", os.Getenv("CAVE_REPO_NAME"), "Repository name")

	if err := global.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			printUsage(os.Stdout)
			return nil
		}
		return err
	}

	rest := global.Args()
	if len(rest) == 0 {
		printUsage(os.Stderr)
		return errors.New("missing command")
	}

	ownerName, resolvedRepo, err := resolveRepo(*repo, *owner, *repoName)
	if err != nil && needsRepo(rest) {
		return err
	}

	c := &client{
		baseURL: strings.TrimRight(*baseURL, "/"),
		token:   strings.TrimSpace(*token),
		httpClient: &http.Client{},
	}

	switch rest[0] {
	case "issue", "issues":
		return runIssues(c, ownerName, resolvedRepo, rest[1:])
	case "help", "-h", "--help":
		printUsage(os.Stdout)
		return nil
	default:
		printUsage(os.Stderr)
		return fmt.Errorf("unknown command %q", rest[0])
	}
}

func runIssues(c *client, ownerName, repoName string, args []string) error {
	if len(args) == 0 {
		printIssueUsage(os.Stderr)
		return errors.New("missing issue command")
	}

	switch args[0] {
	case "list":
		return runIssueList(c, ownerName, repoName, args[1:])
	case "get":
		return runIssueGet(c, ownerName, repoName, args[1:])
	case "create":
		return runIssueCreate(c, ownerName, repoName, args[1:])
	case "help", "-h", "--help":
		printIssueUsage(os.Stdout)
		return nil
	default:
		printIssueUsage(os.Stderr)
		return fmt.Errorf("unknown issue command %q", args[0])
	}
}

func runIssueList(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("issue list", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	status := fs.String("status", "open", "Issue status filter")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}

	issues, err := c.listIssues(ownerName, repoName, *status)
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, issues)
	}

	w := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	fmt.Fprintln(w, "NUMBER\tSTATUS\tTITLE")
	for _, issue := range issues {
		fmt.Fprintf(w, "#%d\t%s\t%s\n", issue.Number, issue.Status, issue.Title)
	}
	return w.Flush()
}

func runIssueGet(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("issue get", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	if fs.NArg() != 1 {
		return errors.New("usage: cav issue get [flags] <number>")
	}

	issue, err := c.getIssue(ownerName, repoName, fs.Arg(0))
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, issue)
	}

	fmt.Fprintf(os.Stdout, "#%d %s\n", issue.Number, issue.Title)
	fmt.Fprintf(os.Stdout, "Status: %s\n", issue.Status)
	if issue.Body != "" {
		fmt.Fprintln(os.Stdout)
		fmt.Fprintln(os.Stdout, issue.Body)
	}
	return nil
}

func runIssueCreate(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("issue create", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	title := fs.String("title", "", "Issue title")
	body := fs.String("body", "", "Issue body")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}

	if strings.TrimSpace(*title) == "" {
		return errors.New("issue title is required")
	}

	issue, err := c.createIssue(ownerName, repoName, issueCreateRequest{
		Title: strings.TrimSpace(*title),
		Body:  *body,
	})
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, issue)
	}

	fmt.Fprintf(os.Stdout, "Created issue #%d: %s\n", issue.Number, issue.Title)
	return nil
}

func (c *client) listIssues(ownerName, repoName, status string) ([]Issue, error) {
	values := url.Values{}
	if status != "" {
		values.Set("status", status)
	}

	var issues []Issue
	if err := c.doJSON(http.MethodGet, issueURL(c.baseURL, ownerName, repoName, "", values), nil, &issues); err != nil {
		return nil, err
	}
	return issues, nil
}

func (c *client) getIssue(ownerName, repoName, number string) (*Issue, error) {
	var issue Issue
	if err := c.doJSON(http.MethodGet, issueURL(c.baseURL, ownerName, repoName, number, nil), nil, &issue); err != nil {
		return nil, err
	}
	return &issue, nil
}

func (c *client) createIssue(ownerName, repoName string, payload issueCreateRequest) (*Issue, error) {
	var issue Issue
	if err := c.doJSON(http.MethodPost, issueURL(c.baseURL, ownerName, repoName, "", nil), payload, &issue); err != nil {
		return nil, err
	}
	return &issue, nil
}

func (c *client) doJSON(method, rawURL string, payload any, out any) error {
	var body io.Reader
	if payload != nil {
		buf := &bytes.Buffer{}
		if err := json.NewEncoder(buf).Encode(payload); err != nil {
			return err
		}
		body = buf
	}

	req, err := http.NewRequest(method, rawURL, body)
	if err != nil {
		return err
	}
	req.Header.Set("Accept", "application/json")
	if payload != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	if c.token != "" {
		req.Header.Set("Authorization", "Bearer "+c.token)
	}

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return decodeAPIError(resp)
	}
	if out == nil {
		return nil
	}
	return json.NewDecoder(resp.Body).Decode(out)
}

func decodeAPIError(resp *http.Response) error {
	var apiErr apiError
	if err := json.NewDecoder(resp.Body).Decode(&apiErr); err == nil && apiErr.Error != "" {
		return fmt.Errorf("%s: %s", resp.Status, apiErr.Error)
	}
	return fmt.Errorf("request failed: %s", resp.Status)
}

func resolveRepo(repo, owner, repoName string) (string, string, error) {
	if repo != "" {
		parts := strings.SplitN(repo, "/", 2)
		if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
			return "", "", fmt.Errorf("invalid repo %q, expected OWNER/REPO", repo)
		}
		return parts[0], parts[1], nil
	}
	if owner != "" && repoName != "" {
		return owner, repoName, nil
	}
	return "", "", errors.New("repository is required; pass --repo OWNER/REPO or set CAVE_REPO")
}

func issueURL(baseURL, ownerName, repoName, number string, query url.Values) string {
	path := fmt.Sprintf("%s/api/v1/repos/%s/%s/issues", baseURL, url.PathEscape(ownerName), url.PathEscape(repoName))
	if number != "" {
		path += "/" + url.PathEscape(number)
	}
	if len(query) > 0 {
		path += "?" + query.Encode()
	}
	return path
}

func needsRepo(args []string) bool {
	return len(args) > 0 && (args[0] == "issue" || args[0] == "issues")
}

func envOrDefault(key, fallback string) string {
	value := os.Getenv(key)
	if value == "" {
		return fallback
	}
	return value
}

func writeJSON(w io.Writer, value any) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(value)
}

func printUsage(w io.Writer) {
	fmt.Fprintln(w, "Usage:")
	fmt.Fprintln(w, "  cav [global flags] issue <command> [flags]")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Global flags:")
	fmt.Fprintln(w, "  --base-url URL     Cave base URL (default: http://localhost:8080)")
	fmt.Fprintln(w, "  --token TOKEN      Cave API token (or CAVE_TOKEN)")
	fmt.Fprintln(w, "  --repo OWNER/REPO  Repository target (or CAVE_REPO)")
	fmt.Fprintln(w, "  --owner OWNER      Repository owner (or CAVE_OWNER)")
	fmt.Fprintln(w, "  --name REPO        Repository name (or CAVE_REPO_NAME)")
	fmt.Fprintln(w)
	printIssueUsage(w)
}

func printIssueUsage(w io.Writer) {
	fmt.Fprintln(w, "Issue commands:")
	fmt.Fprintln(w, "  issue list [--status open|closed] [--json]")
	fmt.Fprintln(w, "  issue get [--json] <number>")
	fmt.Fprintln(w, "  issue create --title TITLE [--body TEXT] [--json]")
}
