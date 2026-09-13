/* Fuzzy file finder: press 't' on any repo page (GitHub-style).
   Reads owner/repo/ref from #file-finder-root, lazily fetches
   /:owner/:repo/filelist/:ref, and opens the picked file's blob view. */
(function () {
  'use strict';

  var root = document.getElementById('file-finder-root');
  if (!root) return;

  var owner = root.dataset.owner, repo = root.dataset.repo, ref = root.dataset.ref;
  var files = null, overlay = null, input = null, list = null;
  var results = [], selected = 0;

  function fuzzyScore(query, path) {
    // Subsequence match; higher is better. Bonuses: consecutive runs,
    // basename hits, segment starts. Returns -1 when not a match.
    var q = query.toLowerCase(), p = path.toLowerCase();
    var qi = 0, score = 0, run = 0;
    var base = p.lastIndexOf('/') + 1;
    for (var pi = 0; pi < p.length && qi < q.length; pi++) {
      if (p[pi] === q[qi]) {
        run += 1;
        score += 1 + run * 2;
        if (pi >= base) score += 4;
        if (pi === 0 || p[pi - 1] === '/' || p[pi - 1] === '.' || p[pi - 1] === '-' || p[pi - 1] === '_')
          score += 6;
        qi += 1;
      } else {
        run = 0;
      }
    }
    if (qi < q.length) return -1;
    return score - Math.floor(path.length / 8); // mild short-path preference
  }

  function blobUrl(path) {
    return '/' + owner + '/' + repo + '/blob/' + encodeURIComponent(ref) +
           '?path=' + encodeURIComponent(path);
  }

  function render() {
    list.innerHTML = '';
    results.slice(0, 50).forEach(function (r, i) {
      var li = document.createElement('li');
      li.className = 'ff-item' + (i === selected ? ' ff-selected' : '');
      var slash = r.path.lastIndexOf('/');
      var dir = slash >= 0 ? r.path.slice(0, slash + 1) : '';
      var baseSpan = document.createElement('span');
      baseSpan.className = 'ff-base';
      baseSpan.textContent = r.path.slice(slash + 1);
      var dirSpan = document.createElement('span');
      dirSpan.className = 'ff-dir';
      dirSpan.textContent = dir;
      li.appendChild(dirSpan); li.appendChild(baseSpan);
      li.addEventListener('mousedown', function (e) {
        e.preventDefault();
        window.location.href = blobUrl(r.path);
      });
      list.appendChild(li);
    });
  }

  function update() {
    var q = input.value.trim();
    if (!files) { results = []; render(); return; }
    if (!q) {
      results = files.slice(0, 50).map(function (p) { return { path: p, score: 0 }; });
    } else {
      results = [];
      for (var i = 0; i < files.length; i++) {
        var s = fuzzyScore(q, files[i]);
        if (s >= 0) results.push({ path: files[i], score: s });
      }
      results.sort(function (a, b) { return b.score - a.score; });
    }
    selected = 0;
    render();
  }

  function open() {
    if (!overlay) build();
    overlay.style.display = 'flex';
    input.value = '';
    input.focus();
    if (!files) {
      fetch('/' + owner + '/' + repo + '/filelist/' + encodeURIComponent(ref))
        .then(function (r) { return r.json(); })
        .then(function (j) { files = j; update(); })
        .catch(function () { files = []; update(); });
    }
    update();
  }

  function close() { if (overlay) overlay.style.display = 'none'; }

  function build() {
    overlay = document.createElement('div');
    overlay.id = 'ff-overlay';
    var box = document.createElement('div');
    box.id = 'ff-box';
    input = document.createElement('input');
    input.id = 'ff-input';
    input.type = 'text';
    input.placeholder = 'Jump to file… (Esc to close)';
    input.autocomplete = 'off';
    list = document.createElement('ul');
    list.id = 'ff-list';
    box.appendChild(input); box.appendChild(list);
    overlay.appendChild(box);
    document.body.appendChild(overlay);

    overlay.addEventListener('mousedown', function (e) {
      if (e.target === overlay) close();
    });
    input.addEventListener('input', update);
    input.addEventListener('keydown', function (e) {
      if (e.key === 'Escape') { close(); }
      else if (e.key === 'ArrowDown') { e.preventDefault(); selected = Math.min(selected + 1, Math.min(results.length, 50) - 1); render(); }
      else if (e.key === 'ArrowUp') { e.preventDefault(); selected = Math.max(selected - 1, 0); render(); }
      else if (e.key === 'Enter') {
        if (results[selected]) window.location.href = blobUrl(results[selected].path);
      }
    });
  }

  document.addEventListener('keydown', function (e) {
    var tag = (e.target.tagName || '').toLowerCase();
    if (tag === 'input' || tag === 'textarea' || e.target.isContentEditable) return;
    if (e.key === 't' && !e.ctrlKey && !e.metaKey && !e.altKey) {
      e.preventDefault();
      open();
    }
  });
})();
