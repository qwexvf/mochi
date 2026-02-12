// servers/mercurius.js
// Mercurius - high-performance GraphQL for Fastify

import Fastify from 'fastify';
import mercurius from 'mercurius';
import { typeDefs, resolvers } from '../schema.js';

const app = Fastify({ logger: false });

// Register Mercurius
app.register(mercurius, {
  schema: typeDefs,
  resolvers,
  graphiql: false,
  jit: 1, // Enable JIT compilation after 1 request
});

// Health check
app.get('/health', async () => 'ok');

const PORT = process.env.PORT || 4003;

app.listen({ port: PORT, host: '0.0.0.0' }).then(() => {
  console.log(`Mercurius server running at http://localhost:${PORT}/graphql`);
});
