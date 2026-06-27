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
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"text/tabwriter"
)

// openBrowser opens rawURL in the platform's default browser, falling back to
// printing the URL when no opener is available (e.g. headless environments).
func openBrowser(rawURL string) error {
	var cmd string
	var args []string
	switch runtime.GOOS {
	case "darwin":
		cmd = "open"
	case "windows":
		cmd, args = "rundll32", []string{"url.dll,FileProtocolHandler"}
	default:
		cmd = "xdg-open"
	}
	if path, err := exec.LookPath(cmd); err == nil {
		return exec.Command(path, append(args, rawURL)...).Start()
	}
	fmt.Fprintln(os.Stdout, rawURL)
	return nil
}

const defaultBaseURL = "http://localhost:8080"

type Issue struct {
	ID        int64        `json:"id"`
	RepoID    int64        `json:"repo_id"`
	Number    int64        `json:"number"`
	AuthorID  int64        `json:"author_id"`
	Title     string       `json:"title"`
	Body      string       `json:"body"`
	Status    string       `json:"status"`
	Author    string       `json:"author"`
	CreatedAt json.Number  `json:"created_at"`
	UpdatedAt json.Number  `json:"updated_at"`
	ClosedAt  *json.Number `json:"closed_at"`
	Comments  []IssueComment `json:"comments"`
}

type issueCreateRequest struct {
	Title string `json:"title"`
	Body  string `json:"body,omitempty"`
}

type issueUpdateRequest struct {
	Status string `json:"status"`
}

type issueCommentRequest struct {
	Body string `json:"body"`
}

type IssueComment struct {
	ID        int64       `json:"id"`
	IssueID   int64       `json:"issue_id"`
	AuthorID  int64       `json:"author_id"`
	Author    string      `json:"author"`
	Body      string      `json:"body"`
	CreatedAt json.Number `json:"created_at"`
}

type Repo struct {
	ID          int64       `json:"id"`
	Name        string      `json:"name"`
	OwnerID     *int64      `json:"owner_id"`
	OrgID       *int64      `json:"org_id"`
	OwnerName   string      `json:"owner_name"`
	Description string      `json:"description"`
	IsPrivate   bool        `json:"is_private"`
	CreatedAt   json.Number `json:"created_at"`
}

type repoCreateRequest struct {
	Name                  string `json:"name,omitempty"`
	Description           string `json:"description,omitempty"`
	Private               bool   `json:"private,omitempty"`
	Mode                  string `json:"mode,omitempty"`
	URL                   string `json:"url,omitempty"`
	AuthToken             string `json:"auth_token,omitempty"`
	MirrorIntervalMinutes int    `json:"mirror_interval_minutes,omitempty"`
}

type apiError struct {
	Error string `json:"error"`
}

type config struct {
	BaseURL string `json:"base_url,omitempty"`
	Token   string `json:"token,omitempty"`
}

type client struct {
	baseURL    string
	token      string
	httpClient *http.Client
}

func configPath() string {
	if xdg := os.Getenv("XDG_CONFIG_HOME"); xdg != "" {
		return filepath.Join(xdg, "cave", "config.json")
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".config", "cave", "config.json")
}

func loadConfig() config {
	var cfg config
	data, err := os.ReadFile(configPath())
	if err != nil {
		return cfg
	}
	json.Unmarshal(data, &cfg)
	return cfg
}

func saveConfig(cfg config) error {
	p := configPath()
	if err := os.MkdirAll(filepath.Dir(p), 0700); err != nil {
		return err
	}
	data, err := json.MarshalIndent(cfg, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(p, append(data, '\n'), 0600)
}

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintf(os.Stderr, "cave: %v\n", err)
		os.Exit(1)
	}
}

func run(args []string) error {
	if len(args) == 0 {
		printUsage(os.Stderr)
		return errors.New("missing command")
	}

	// Handle login/logout before flag parsing — they don't need global flags
	if args[0] == "login" {
		return runLogin(args[1:])
	}
	if args[0] == "logout" {
		return runLogout()
	}
	if args[0] == "status" {
		return runStatus()
	}

	cfg := loadConfig()

	global := flag.NewFlagSet("cave", flag.ContinueOnError)
	global.SetOutput(io.Discard)
	baseURL := global.String("base-url", envOrDefault("CAVE_BASE_URL", firstNonEmpty(cfg.BaseURL, defaultBaseURL)), "Cave base URL")
	token := global.String("token", firstNonEmpty(os.Getenv("CAVE_TOKEN"), cfg.Token), "Cave API token")
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
		baseURL:    strings.TrimRight(*baseURL, "/"),
		token:      strings.TrimSpace(*token),
		httpClient: &http.Client{},
	}

	switch rest[0] {
	case "issue", "issues":
		return runIssues(c, ownerName, resolvedRepo, rest[1:])
	case "repo", "repos":
		return runRepos(c, rest[1:])
	case "deps":
		return runDeps(c, ownerName, resolvedRepo, rest[1:])
	case "help", "-h", "--help":
		printUsage(os.Stdout)
		return nil
	default:
		printUsage(os.Stderr)
		return fmt.Errorf("unknown command %q", rest[0])
	}
}

func runRepos(c *client, args []string) error {
	if len(args) == 0 {
		printRepoUsage(os.Stderr)
		return errors.New("missing repo command")
	}
	switch args[0] {
	case "create":
		return runRepoCreate(c, args[1:])
	case "help", "-h", "--help":
		printRepoUsage(os.Stdout)
		return nil
	default:
		printRepoUsage(os.Stderr)
		return fmt.Errorf("unknown repo command %q", args[0])
	}
}

func runRepoCreate(c *client, args []string) error {
	fs := flag.NewFlagSet("repo create", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	description := fs.String("description", "", "Repo description")
	private := fs.Bool("private", false, "Make the repo private")
	mode := fs.String("mode", "empty", "One of: empty, import, mirror")
	remoteURL := fs.String("url", "", "Remote URL (required for import/mirror)")
	authToken := fs.String("auth-token", "", "Auth token for the remote URL (optional)")
	interval := fs.Int("mirror-interval", 60, "Pull interval in minutes (mirror mode)")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")

	// Allow flags interleaved with the positional name: re-invoke Parse
	// after each non-flag token. Stdlib flag stops at the first positional,
	// so without this, `cave repo create foo --json` silently drops --json.
	var positional []string
	remaining := args
	for {
		if err := fs.Parse(remaining); err != nil {
			if errors.Is(err, flag.ErrHelp) {
				return nil
			}
			return err
		}
		rest := fs.Args()
		if len(rest) == 0 {
			break
		}
		positional = append(positional, rest[0])
		remaining = rest[1:]
	}

	name := ""
	if len(positional) > 0 {
		name = strings.TrimSpace(positional[0])
	}
	if name == "" && (*mode == "import" || *mode == "mirror") && *remoteURL != "" {
		// Server will derive name from URL.
	} else if name == "" {
		return errors.New("usage: cave repo create <name> [flags]")
	}

	repo, err := c.createRepo(repoCreateRequest{
		Name:                  name,
		Description:           strings.TrimSpace(*description),
		Private:               *private,
		Mode:                  *mode,
		URL:                   strings.TrimSpace(*remoteURL),
		AuthToken:             strings.TrimSpace(*authToken),
		MirrorIntervalMinutes: *interval,
	})
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, repo)
	}

	owner := repoOwnerLabel(repo)
	fmt.Fprintf(os.Stdout, "Created %s/%s\n", owner, repo.Name)
	fmt.Fprintf(os.Stdout, "  %s/%s/%s\n",
		strings.TrimRight(c.baseURL, "/"), owner, repo.Name)
	return nil
}

func repoOwnerLabel(r *Repo) string {
	if r.OwnerName != "" {
		return r.OwnerName
	}
	// Belt-and-braces — the server now always returns owner_name, but if a
	// future version doesn't, we'd rather print something than crash.
	if r.OwnerID != nil {
		return fmt.Sprintf("(owner_id=%d)", *r.OwnerID)
	}
	if r.OrgID != nil {
		return fmt.Sprintf("(org_id=%d)", *r.OrgID)
	}
	return "?"
}

func (c *client) createRepo(payload repoCreateRequest) (*Repo, error) {
	var repo Repo
	if err := c.doJSON(http.MethodPost, c.baseURL+"/api/v1/user/repos", payload, &repo); err != nil {
		return nil, err
	}
	return &repo, nil
}

func runLogin(args []string) error {
	fs := flag.NewFlagSet("login", flag.ContinueOnError)
	baseURL := fs.String("base-url", "", "Cave base URL")
	token := fs.String("token", "", "Cave API token")
	if err := fs.Parse(args); err != nil {
		return err
	}

	cfg := loadConfig()

	if *baseURL != "" {
		cfg.BaseURL = strings.TrimRight(*baseURL, "/")
	} else if cfg.BaseURL == "" {
		cfg.BaseURL = defaultBaseURL
	}

	if *token != "" {
		cfg.Token = *token
	} else {
		fmt.Fprint(os.Stderr, "Token: ")
		var t string
		if _, err := fmt.Scanln(&t); err != nil {
			return errors.New("token is required")
		}
		cfg.Token = strings.TrimSpace(t)
	}

	if err := saveConfig(cfg); err != nil {
		return fmt.Errorf("saving config: %w", err)
	}

	fmt.Fprintf(os.Stdout, "Logged in to %s\n", cfg.BaseURL)
	fmt.Fprintf(os.Stdout, "Config saved to %s\n", configPath())
	return nil
}

func runLogout() error {
	p := configPath()
	if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
		return err
	}
	fmt.Fprintln(os.Stdout, "Logged out (config removed)")
	return nil
}

func runStatus() error {
	cfg := loadConfig()
	if cfg.Token == "" && cfg.BaseURL == "" {
		fmt.Fprintln(os.Stdout, "Not logged in")
		return nil
	}
	fmt.Fprintf(os.Stdout, "Server:  %s\n", firstNonEmpty(cfg.BaseURL, defaultBaseURL))
	if cfg.Token != "" {
		fmt.Fprintf(os.Stdout, "Token:   %s...%s\n", cfg.Token[:8], cfg.Token[len(cfg.Token)-4:])
	}
	fmt.Fprintf(os.Stdout, "Config:  %s\n", configPath())
	return nil
}

func runIssues(c *client, ownerName, repoName string, args []string) error {
	if len(args) == 0 {
		printIssueUsage(os.Stderr)
		return errors.New("missing issue command")
	}

	switch args[0] {
	case "list":
		return runIssueList(c, ownerName, repoName, args[1:])
	case "view", "get":
		return runIssueView(c, ownerName, repoName, args[1:])
	case "create":
		return runIssueCreate(c, ownerName, repoName, args[1:])
	case "comment":
		return runIssueComment(c, ownerName, repoName, args[1:])
	case "close":
		return runIssueClose(c, ownerName, repoName, args[1:])
	case "reopen":
		return runIssueReopen(c, ownerName, repoName, args[1:])
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
	status := fs.String("status", "open", "Issue status filter (open|closed)")
	state := fs.String("state", "", "Issue state filter (open|closed) [gh alias for --status]")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	// gh uses --state; accept it as an alias and let it win when set.
	filter := *status
	if *state != "" {
		filter = *state
	}

	issues, err := c.listIssues(ownerName, repoName, filter)
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

func runIssueView(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("issue view", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	jsonOut := fs.Bool("json", false, "Emit raw JSON")
	comments := fs.Bool("comments", false, "Include the full comment thread")
	web := fs.Bool("web", false, "Open the issue in a browser")
	if err := fs.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}
	if fs.NArg() != 1 {
		return errors.New("usage: cave issue view [flags] <number>")
	}
	number := fs.Arg(0)

	if *web {
		u := fmt.Sprintf("%s/%s/%s/issues/%s", c.baseURL,
			url.PathEscape(ownerName), url.PathEscape(repoName), url.PathEscape(number))
		return openBrowser(u)
	}

	issue, err := c.getIssue(ownerName, repoName, number)
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, issue)
	}

	// Header: title, then a gh-style status line.
	fmt.Fprintf(os.Stdout, "#%d %s\n", issue.Number, issue.Title)
	openedBy := ""
	if issue.Author != "" {
		openedBy = fmt.Sprintf(" • opened by %s", issue.Author)
	}
	fmt.Fprintf(os.Stdout, "%s%s • %d comment%s\n",
		strings.ToUpper(issue.Status), openedBy,
		len(issue.Comments), plural(len(issue.Comments)))
	if issue.Body != "" {
		fmt.Fprintln(os.Stdout)
		fmt.Fprintln(os.Stdout, issue.Body)
	}

	if *comments {
		for _, cm := range issue.Comments {
			fmt.Fprintln(os.Stdout, "\n--------------------------------------------------")
			author := cm.Author
			if author == "" {
				author = "unknown"
			}
			fmt.Fprintf(os.Stdout, "%s commented:\n\n", author)
			fmt.Fprintln(os.Stdout, cm.Body)
		}
	} else if len(issue.Comments) > 0 {
		fmt.Fprintf(os.Stdout, "\n———\nUse --comments to read %d comment%s.\n",
			len(issue.Comments), plural(len(issue.Comments)))
	}
	return nil
}

func plural(n int) string {
	if n == 1 {
		return ""
	}
	return "s"
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

func runIssueComment(c *client, ownerName, repoName string, args []string) error {
	fs := flag.NewFlagSet("issue comment", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	body := fs.String("body", "", "Comment body (use - to read stdin)")
	bodyFile := fs.String("body-file", "", "Read comment body from file")
	jsonOut := fs.Bool("json", false, "Emit raw JSON")

	// Allow flags interleaved with the positional number.
	var positional []string
	remaining := args
	for {
		if err := fs.Parse(remaining); err != nil {
			if errors.Is(err, flag.ErrHelp) {
				return nil
			}
			return err
		}
		rest := fs.Args()
		if len(rest) == 0 {
			break
		}
		positional = append(positional, rest[0])
		remaining = rest[1:]
	}

	if len(positional) != 1 {
		return errors.New("usage: cave issue comment <number> --body TEXT [--json]")
	}
	number := positional[0]

	text, err := resolveCommentBody(*body, *bodyFile)
	if err != nil {
		return err
	}
	if strings.TrimSpace(text) == "" {
		return errors.New("comment body is required")
	}

	comment, err := c.createIssueComment(ownerName, repoName, number, issueCommentRequest{Body: text})
	if err != nil {
		return err
	}

	if *jsonOut {
		return writeJSON(os.Stdout, comment)
	}

	fmt.Fprintf(os.Stdout, "Commented on issue #%s\n", number)
	return nil
}

func resolveCommentBody(body, bodyFile string) (string, error) {
	if bodyFile != "" && body != "" {
		return "", errors.New("--body and --body-file are mutually exclusive")
	}
	if bodyFile != "" {
		data, err := os.ReadFile(bodyFile)
		if err != nil {
			return "", err
		}
		return string(data), nil
	}
	if body == "-" {
		data, err := io.ReadAll(os.Stdin)
		if err != nil {
			return "", err
		}
		return string(data), nil
	}
	return body, nil
}

func (c *client) createIssueComment(ownerName, repoName, number string, payload issueCommentRequest) (*IssueComment, error) {
	var comment IssueComment
	rawURL := issueURL(c.baseURL, ownerName, repoName, number, nil) + "/comments"
	if err := c.doJSON(http.MethodPost, rawURL, payload, &comment); err != nil {
		return nil, err
	}
	return &comment, nil
}

func runIssueClose(c *client, ownerName, repoName string, args []string) error {
	if len(args) != 1 {
		return errors.New("usage: cave issue close <number>")
	}
	issue, err := c.updateIssue(ownerName, repoName, args[0], issueUpdateRequest{Status: "closed"})
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stdout, "Closed issue #%d: %s\n", issue.Number, issue.Title)
	return nil
}

func runIssueReopen(c *client, ownerName, repoName string, args []string) error {
	if len(args) != 1 {
		return errors.New("usage: cave issue reopen <number>")
	}
	issue, err := c.updateIssue(ownerName, repoName, args[0], issueUpdateRequest{Status: "open"})
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stdout, "Reopened issue #%d: %s\n", issue.Number, issue.Title)
	return nil
}

func (c *client) updateIssue(ownerName, repoName, number string, payload issueUpdateRequest) (*Issue, error) {
	var issue Issue
	if err := c.doJSON(http.MethodPatch, issueURL(c.baseURL, ownerName, repoName, number, nil), payload, &issue); err != nil {
		return nil, err
	}
	return &issue, nil
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
	if len(args) == 0 {
		return false
	}
	switch args[0] {
	case "issue", "issues":
		return true
	case "deps":
		// Only the repo-scoped deps subcommands need a repo (not who-uses/help).
		return len(args) >= 2 &&
			(args[1] == "list" || args[1] == "alerts" || args[1] == "dismiss")
	default:
		return false
	}
}

func envOrDefault(key, fallback string) string {
	value := os.Getenv(key)
	if value == "" {
		return fallback
	}
	return value
}

func firstNonEmpty(vals ...string) string {
	for _, v := range vals {
		if v != "" {
			return v
		}
	}
	return ""
}

func writeJSON(w io.Writer, value any) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(value)
}

func printUsage(w io.Writer) {
	fmt.Fprintln(w, "Usage:")
	fmt.Fprintln(w, "  cave [global flags] <command> [flags]")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Global flags:")
	fmt.Fprintln(w, "  --base-url URL     Cave base URL (default: http://localhost:8080)")
	fmt.Fprintln(w, "  --token TOKEN      Cave API token (or CAVE_TOKEN)")
	fmt.Fprintln(w, "  --repo OWNER/REPO  Repository target (or CAVE_REPO)")
	fmt.Fprintln(w, "  --owner OWNER      Repository owner (or CAVE_OWNER)")
	fmt.Fprintln(w, "  --name REPO        Repository name (or CAVE_REPO_NAME)")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "Commands:")
	fmt.Fprintln(w, "  login [--base-url URL] [--token TOKEN]   Save server URL and token")
	fmt.Fprintln(w, "  logout                                   Remove saved config")
	fmt.Fprintln(w, "  status                                   Show current auth config")
	fmt.Fprintln(w)
	printRepoUsage(w)
	fmt.Fprintln(w)
	printIssueUsage(w)
	fmt.Fprintln(w)
	printDepsUsage(w)
}

func printRepoUsage(w io.Writer) {
	fmt.Fprintln(w, "Repo commands:")
	fmt.Fprintln(w, "  repo create <name> [--description \"…\"] [--private]")
	fmt.Fprintln(w, "                       [--mode empty|import|mirror]")
	fmt.Fprintln(w, "                       [--url URL] [--auth-token TOKEN]")
	fmt.Fprintln(w, "                       [--mirror-interval MINUTES] [--json]")
}

func printIssueUsage(w io.Writer) {
	fmt.Fprintln(w, "Issue commands:")
	fmt.Fprintln(w, "  issue list [--state open|closed] [--json]")
	fmt.Fprintln(w, "  issue view [--comments] [--web] [--json] <number>")
	fmt.Fprintln(w, "  issue create --title TITLE [--body TEXT] [--json]")
	fmt.Fprintln(w, "  issue comment <number> --body TEXT|- [--body-file PATH] [--json]")
	fmt.Fprintln(w, "  issue close <number>")
	fmt.Fprintln(w, "  issue reopen <number>")
}
