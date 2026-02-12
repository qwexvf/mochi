// mochi/playground.gleam
// GraphiQL and GraphQL Playground HTML generators

// ============================================================================
// GraphiQL
// ============================================================================

/// Generate GraphiQL HTML page
pub fn graphiql(endpoint: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
  <title>GraphiQL - Mochi</title>
  <style>
    body {
      height: 100%;
      margin: 0;
      width: 100%;
      overflow: hidden;
    }
    #graphiql {
      height: 100vh;
    }
  </style>
  <link rel=\"stylesheet\" href=\"https://unpkg.com/graphiql@3/graphiql.min.css\" />
</head>
<body>
  <div id=\"graphiql\">Loading...</div>
  <script src=\"https://unpkg.com/react@18/umd/react.production.min.js\" crossorigin></script>
  <script src=\"https://unpkg.com/react-dom@18/umd/react-dom.production.min.js\" crossorigin></script>
  <script src=\"https://unpkg.com/graphiql@3/graphiql.min.js\" crossorigin></script>
  <script>
    const fetcher = GraphiQL.createFetcher({
      url: '" <> endpoint <> "',
    });
    const root = ReactDOM.createRoot(document.getElementById('graphiql'));
    root.render(
      React.createElement(GraphiQL, {
        fetcher,
        defaultEditorToolsVisibility: true,
      }),
    );
  </script>
</body>
</html>"
}

// ============================================================================
// GraphQL Playground (Legacy)
// ============================================================================

/// Generate GraphQL Playground HTML page (legacy, but still popular)
pub fn playground(endpoint: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\" />
  <title>GraphQL Playground - Mochi</title>
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/graphql-playground-react/build/static/css/index.css\" />
  <link rel=\"shortcut icon\" href=\"https://cdn.jsdelivr.net/npm/graphql-playground-react/build/favicon.png\" />
  <script src=\"https://cdn.jsdelivr.net/npm/graphql-playground-react/build/static/js/middleware.js\"></script>
</head>
<body>
  <div id=\"root\">
    <style>
      body {
        background-color: rgb(23, 42, 58);
        font-family: Open Sans, sans-serif;
        height: 90vh;
      }
      #root {
        height: 100%;
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .loading {
        font-size: 32px;
        font-weight: 200;
        color: rgba(255, 255, 255, .6);
        margin-left: 28px;
      }
      img {
        width: 78px;
        height: 78px;
      }
      .title {
        font-weight: 400;
      }
    </style>
    <img src=\"https://cdn.jsdelivr.net/npm/graphql-playground-react/build/logo.png\" alt=\"\" />
    <div class=\"loading\">
      Loading <span class=\"title\">GraphQL Playground</span>
    </div>
  </div>
  <script>
    window.addEventListener('load', function () {
      GraphQLPlayground.init(document.getElementById('root'), {
        endpoint: '" <> endpoint <> "',
        settings: {
          'editor.theme': 'dark',
          'editor.cursorShape': 'line',
          'editor.fontSize': 14,
          'editor.fontFamily': \"'Source Code Pro', 'Consolas', 'Inconsolata', 'Droid Sans Mono', 'Monaco', monospace\",
          'request.credentials': 'same-origin',
        }
      });
    });
  </script>
</body>
</html>"
}

// ============================================================================
// Apollo Sandbox
// ============================================================================

/// Generate Apollo Sandbox HTML page (modern alternative)
pub fn apollo_sandbox(endpoint: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\" />
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
  <title>Apollo Sandbox - Mochi</title>
  <style>
    body {
      margin: 0;
      padding: 0;
      height: 100vh;
      width: 100vw;
    }
  </style>
</head>
<body>
  <div style=\"width: 100%; height: 100%;\" id=\"sandbox\"></div>
  <script src=\"https://embeddable-sandbox.cdn.apollographql.com/_latest/embeddable-sandbox.umd.production.min.js\"></script>
  <script>
    new window.EmbeddedSandbox({
      target: '#sandbox',
      initialEndpoint: '" <> endpoint <> "',
    });
  </script>
</body>
</html>"
}

// ============================================================================
// Simple Explorer (No external dependencies)
// ============================================================================

/// Generate a simple GraphQL explorer with no external dependencies
pub fn simple_explorer(endpoint: String) -> String {
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>Mochi GraphQL Explorer</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: #1a1a2e; color: #eee; height: 100vh;
      display: flex; flex-direction: column;
    }
    header {
      background: #16213e; padding: 1rem 2rem;
      display: flex; align-items: center; gap: 1rem;
    }
    header h1 { font-size: 1.2rem; font-weight: 500; }
    header span { color: #888; font-size: 0.9rem; }
    main { display: flex; flex: 1; overflow: hidden; }
    .panel { flex: 1; display: flex; flex-direction: column; }
    .panel-header {
      padding: 0.5rem 1rem; background: #0f3460;
      font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.05em;
    }
    textarea {
      flex: 1; background: #1a1a2e; color: #eee; border: none;
      font-family: 'Monaco', 'Menlo', monospace; font-size: 14px;
      padding: 1rem; resize: none; outline: none;
    }
    .divider { width: 4px; background: #0f3460; cursor: col-resize; }
    button {
      background: #e94560; color: white; border: none;
      padding: 0.5rem 1.5rem; border-radius: 4px; cursor: pointer;
      font-weight: 500; transition: background 0.2s;
    }
    button:hover { background: #ff6b6b; }
    button:disabled { background: #555; cursor: not-allowed; }
    pre {
      flex: 1; overflow: auto; padding: 1rem; margin: 0;
      font-family: 'Monaco', 'Menlo', monospace; font-size: 14px;
      white-space: pre-wrap; word-break: break-word;
    }
    .error { color: #ff6b6b; }
    .success { color: #6bff6b; }
    .loading { color: #ffbb33; }
  </style>
</head>
<body>
  <header>
    <h1>🍡 Mochi GraphQL</h1>
    <span>|</span>
    <span id=\"endpoint\">" <> endpoint <> "</span>
    <div style=\"flex:1\"></div>
    <button onclick=\"execute()\" id=\"runBtn\">▶ Run Query</button>
  </header>
  <main>
    <div class=\"panel\">
      <div class=\"panel-header\">Query</div>
      <textarea id=\"query\" placeholder=\"Enter your GraphQL query...\">{
  users {
    id
    name
    email
  }
}</textarea>
    </div>
    <div class=\"divider\"></div>
    <div class=\"panel\">
      <div class=\"panel-header\">Variables (JSON)</div>
      <textarea id=\"variables\" placeholder=\"{}\">{}</textarea>
    </div>
    <div class=\"divider\"></div>
    <div class=\"panel\">
      <div class=\"panel-header\">Response</div>
      <pre id=\"response\">Click \"Run Query\" to execute</pre>
    </div>
  </main>
  <script>
    const endpoint = '" <> endpoint <> "';

    async function execute() {
      const btn = document.getElementById('runBtn');
      const response = document.getElementById('response');
      const query = document.getElementById('query').value;
      let variables = {};

      try {
        const varsText = document.getElementById('variables').value.trim();
        if (varsText) variables = JSON.parse(varsText);
      } catch (e) {
        response.className = 'error';
        response.textContent = 'Invalid JSON in variables: ' + e.message;
        return;
      }

      btn.disabled = true;
      btn.textContent = '⏳ Running...';
      response.className = 'loading';
      response.textContent = 'Executing query...';

      try {
        const res = await fetch(endpoint, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ query, variables })
        });
        const data = await res.json();
        response.className = data.errors ? 'error' : 'success';
        response.textContent = JSON.stringify(data, null, 2);
      } catch (e) {
        response.className = 'error';
        response.textContent = 'Request failed: ' + e.message;
      } finally {
        btn.disabled = false;
        btn.textContent = '▶ Run Query';
      }
    }

    // Ctrl+Enter to execute
    document.addEventListener('keydown', (e) => {
      if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') execute();
    });
  </script>
</body>
</html>"
}
