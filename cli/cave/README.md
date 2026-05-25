# cave

`cave` is a small Go client for Cave's HTTP API.

Current commands match the API surface available in this repository:

- `issue list`
- `issue get`
- `issue create`

Examples:

```sh
go run ./cli/cave --repo admin/test-browse issue list
go run ./cli/cave --repo admin/test-browse issue get 1
go run ./cli/cave --repo admin/test-browse --token "$CAVE_TOKEN" issue create --title "Bug" --body "Details"
```
