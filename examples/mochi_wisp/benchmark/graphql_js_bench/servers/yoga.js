// servers/yoga.js
// GraphQL Yoga - modern, feature-rich GraphQL server

import { createServer } from 'node:http';
import { createYoga, createSchema } from 'graphql-yoga';
import { typeDefs, resolvers } from '../schema.js';

const yoga = createYoga({
  schema: createSchema({
    typeDefs,
    resolvers,
  }),
  graphiql: false,
  logging: false,
});

const server = createServer(yoga);

const PORT = process.env.PORT || 4004;

server.listen(PORT, '0.0.0.0', () => {
  console.log(`GraphQL Yoga server running at http://localhost:${PORT}/graphql`);
});
