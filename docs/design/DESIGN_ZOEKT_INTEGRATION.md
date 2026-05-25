# Design: High-Performance Search with Zoekt Sidecar

To support scaling "way beyond" thousands of repositories, Cave will integrate **Zoekt** as a dedicated search sidecar.

## 1. Architectural Components

*   **Zoekt Indexer (zoekt-indexserver):** A sidecar container that watches or is notified of Git repository changes. It builds and maintains the trigram index on disk.
*   **Zoekt Webserver (zoekt-webserver):** A sidecar container that serves a JSON/HTTP API for searching the index.
*   **Cave (Lisp):** Acts as the frontend, providing the search UI and querying the Zoekt API.

## 2. Implementation Phases

### Phase 1: Deployment (Quadlet)
Add two new Quadlet units to deploy/quadlet/:
*   cave-zoekt-index.container: Runs sourcegraph/zoekt-indexserver. Mounts the same cave-data.volume as Cave to access bare Git repositories.
*   cave-zoekt-web.container: Runs sourcegraph/zoekt-webserver. Exposes a port (default 6070) internally to the cave.network.

### Phase 2: Indexing Trigger
Update handle-post-receive in src/main.lisp to notify Zoekt of updates:
*   Zoekt can be configured to watch directories, but for immediate updates, we can explicitly call zoekt-git-index or rely on Zoekt's filesystem watcher.
*   **Optimization:** When a repo is created or a push occurs, Cave sends a "Re-index" signal to the indexer.

### Phase 3: Zoekt API Client
Implement a new module src/search-zoekt.lisp:
*   Function zoekt-search (query &key limit repos): Sends a POST request to http://cave-zoekt-web:6070/search with the JSON payload.
*   Parse Zoekt's rich response (file matches, line numbers, highlighted fragments).

### Phase 4: UI Integration
*   **Global Search Bar:** Add to the page macro in src/views.lisp.
*   **Results Page:** A new view function view-search-results that displays matches categorized by repository.

## 3. Data Flow
1.  **Push:** Developer pushes to org/repo.
2.  **Hook:** cave-server post-receive runs, updating the DB and potentially touching a "last-updated" file that Zoekt watches.
3.  **Index:** zoekt-indexserver detects the change and updates the trigram index for org/repo.
4.  **Search:** User types a query in Cave. Cave sends JSON request to zoekt-webserver.
5.  **Render:** Cave receives JSON results and renders them using Spinneret.

## 4. Why this scales
*   **Memory Efficiency:** Zoekt uses a compact index; 100GB of code results in ~20GB of index.
*   **Speed:** Trigram matching is O(log n) or better for most queries, avoiding full file scans.
*   **Isolation:** Heavy indexing work happens in a separate process/container, not impacting the main web server's responsiveness.
