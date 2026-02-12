// servers/apollo.js
// Apollo Server 4 with Express

import express from 'express';
import { ApolloServer } from '@apollo/server';
import { expressMiddleware } from '@apollo/server/express4';
import { typeDefs, resolvers } from '../schema.js';

async function main() {
  const app = express();
  app.use(express.json());

  // Create Apollo Server
  const server = new ApolloServer({
    typeDefs,
    resolvers,
    // Disable introspection and playground for benchmarks
    introspection: false,
  });

  await server.start();

  // Health check
  app.get('/health', (req, res) => res.send('ok'));

  // GraphQL endpoint
  app.use('/graphql', expressMiddleware(server));

  const PORT = process.env.PORT || 4002;
  app.listen(PORT, '0.0.0.0', () => {
    console.log(`Apollo Server running at http://localhost:${PORT}/graphql`);
  });
}

main().catch(console.error);
