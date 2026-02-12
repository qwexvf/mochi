#!/usr/bin/env node
// benchmark.js
// Benchmarks all GraphQL servers using autocannon

import autocannon from 'autocannon';

// ============================================================================
// Test Queries (matching Mochi benchmarks)
// ============================================================================

const queries = {
  simple: {
    name: 'Simple Query',
    query: '{ users { id name } }',
  },

  medium: {
    name: 'Medium Query (nested)',
    query: `{
      users {
        id
        name
        email
        role
        posts {
          id
          title
        }
      }
    }`,
  },

  complex: {
    name: 'Complex Query (deeply nested)',
    query: `{
      users {
        id
        name
        email
        role
        createdAt
        posts {
          id
          title
          content
          status
          createdAt
          comments {
            id
            content
            author {
              id
              name
            }
          }
        }
        comments {
          id
          content
          post {
            title
          }
        }
      }
    }`,
  },

  withArgs: {
    name: 'Query with Arguments',
    query: '{ user(id: "1") { id name email posts { title } } }',
  },

  pagination: {
    name: 'Pagination Query',
    query: `{
      usersConnection(first: 3, offset: 0) {
        nodes { id name email }
        totalCount
      }
      postsConnection(first: 5, offset: 0) {
        nodes { id title status }
        totalCount
      }
    }`,
  },

  mutation: {
    name: 'Mutation',
    query: `mutation {
      createPost(input: { title: "Test", content: "Test content", authorId: "1" }) {
        id
        title
        author { name }
      }
    }`,
  },
};

// ============================================================================
// Server Configuration
// ============================================================================

const servers = [
  { name: 'graphql-js', port: 4001, url: 'http://localhost:4001/graphql' },
  { name: 'Apollo Server', port: 4002, url: 'http://localhost:4002/graphql' },
  { name: 'Mercurius', port: 4003, url: 'http://localhost:4003/graphql' },
  { name: 'GraphQL Yoga', port: 4004, url: 'http://localhost:4004/graphql' },
  { name: 'Mochi (Gleam)', port: 8000, url: 'http://localhost:8000/graphql' },
];

// ============================================================================
// Benchmark Configuration
// ============================================================================

const isQuick = process.argv.includes('--quick');

const config = {
  duration: isQuick ? 5 : 10,
  connections: 10,
  pipelining: 1,
};

// ============================================================================
// Helpers
// ============================================================================

async function checkServer(server) {
  try {
    const res = await fetch(server.url.replace('/graphql', '/health'));
    return res.ok;
  } catch {
    return false;
  }
}

async function runBenchmark(server, query) {
  return new Promise((resolve, reject) => {
    const instance = autocannon({
      url: server.url,
      connections: config.connections,
      pipelining: config.pipelining,
      duration: config.duration,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ query: query.query }),
    }, (err, result) => {
      if (err) reject(err);
      else resolve(result);
    });
  });
}

function formatNumber(n) {
  return n.toLocaleString();
}

function formatLatency(ms) {
  if (ms < 1) return `${(ms * 1000).toFixed(0)} µs`;
  return `${ms.toFixed(2)} ms`;
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════════╗');
  console.log('║         Node.js GraphQL Benchmark Suite                        ║');
  console.log('║         Comparing: graphql-js, Apollo, Mercurius, Yoga, Mochi  ║');
  console.log('╚════════════════════════════════════════════════════════════════╝');
  console.log();
  console.log(`Configuration: ${config.duration}s duration, ${config.connections} connections`);
  console.log();

  // Check which servers are running
  console.log('Checking servers...');
  const availableServers = [];
  for (const server of servers) {
    const isUp = await checkServer(server);
    if (isUp) {
      console.log(`  ✓ ${server.name} (port ${server.port})`);
      availableServers.push(server);
    } else {
      console.log(`  ✗ ${server.name} (port ${server.port}) - NOT RUNNING`);
    }
  }

  if (availableServers.length === 0) {
    console.log('\nNo servers are running. Start servers first:');
    console.log('  npm run start:graphql-js   # Port 4001');
    console.log('  npm run start:apollo       # Port 4002');
    console.log('  npm run start:mercurius    # Port 4003');
    console.log('  npm run start:yoga         # Port 4004');
    console.log('  cd ../.. && gleam run      # Port 8000 (Mochi)');
    process.exit(1);
  }

  console.log();

  // Results storage
  const results = {};

  // Run benchmarks for each query type
  for (const [queryKey, query] of Object.entries(queries)) {
    console.log(`\n${'═'.repeat(60)}`);
    console.log(`Benchmark: ${query.name}`);
    console.log(`${'═'.repeat(60)}`);

    results[queryKey] = {};

    for (const server of availableServers) {
      process.stdout.write(`  Testing ${server.name}...`);

      try {
        const result = await runBenchmark(server, query);
        results[queryKey][server.name] = {
          requests: result.requests.total,
          throughput: result.requests.average,
          latencyAvg: result.latency.average,
          latencyP99: result.latency.p99,
          errors: result.errors + result.timeouts,
        };
        console.log(` ${formatNumber(result.requests.average)} req/s`);
      } catch (err) {
        console.log(` ERROR: ${err.message}`);
        results[queryKey][server.name] = { error: err.message };
      }
    }
  }

  // Print summary table
  console.log('\n');
  console.log('╔════════════════════════════════════════════════════════════════════════════════╗');
  console.log('║                              RESULTS SUMMARY                                    ║');
  console.log('╚════════════════════════════════════════════════════════════════════════════════╝');
  console.log();

  // Header
  const serverNames = availableServers.map(s => s.name);
  console.log('Query Type'.padEnd(25) + serverNames.map(n => n.padStart(15)).join(''));
  console.log('─'.repeat(25 + serverNames.length * 15));

  // Throughput comparison
  console.log('\nThroughput (req/s):');
  for (const [queryKey, query] of Object.entries(queries)) {
    let line = query.name.slice(0, 24).padEnd(25);
    for (const server of availableServers) {
      const r = results[queryKey][server.name];
      const value = r?.throughput ? formatNumber(Math.round(r.throughput)) : 'N/A';
      line += value.padStart(15);
    }
    console.log(line);
  }

  // Latency comparison
  console.log('\nAvg Latency:');
  for (const [queryKey, query] of Object.entries(queries)) {
    let line = query.name.slice(0, 24).padEnd(25);
    for (const server of availableServers) {
      const r = results[queryKey][server.name];
      const value = r?.latencyAvg ? formatLatency(r.latencyAvg) : 'N/A';
      line += value.padStart(15);
    }
    console.log(line);
  }

  // P99 Latency comparison
  console.log('\nP99 Latency:');
  for (const [queryKey, query] of Object.entries(queries)) {
    let line = query.name.slice(0, 24).padEnd(25);
    for (const server of availableServers) {
      const r = results[queryKey][server.name];
      const value = r?.latencyP99 ? formatLatency(r.latencyP99) : 'N/A';
      line += value.padStart(15);
    }
    console.log(line);
  }

  // Find winner for each query
  console.log('\n\nWinners by Query Type:');
  console.log('─'.repeat(60));
  for (const [queryKey, query] of Object.entries(queries)) {
    let best = null;
    let bestThroughput = 0;
    for (const server of availableServers) {
      const r = results[queryKey][server.name];
      if (r?.throughput && r.throughput > bestThroughput) {
        bestThroughput = r.throughput;
        best = server.name;
      }
    }
    if (best) {
      console.log(`  ${query.name.padEnd(30)} → ${best} (${formatNumber(Math.round(bestThroughput))} req/s)`);
    }
  }

  console.log('\n');
}

main().catch(console.error);
