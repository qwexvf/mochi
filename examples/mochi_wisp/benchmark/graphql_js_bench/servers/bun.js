// servers/bun.js
// Bun GraphQL server using graphql-yoga (fastest Bun-compatible server)

import { createYoga } from 'graphql-yoga';
import { makeExecutableSchema } from '@graphql-tools/schema';
import { typeDefs, resolvers } from '../schema.js';

const schema = makeExecutableSchema({ typeDefs, resolvers });

const yoga = createYoga({
  schema,
  graphiql: false,
  logging: false,
});

const PORT = process.env.PORT || 4000;

Bun.serve({
  port: PORT,
  fetch: yoga.fetch,
});

console.log(`Bun GraphQL server running at http://localhost:${PORT}/graphql`);
