// servers/graphql-js.js
// Pure graphql-js with graphql-http (minimal overhead)

import express from 'express';
import { createHandler } from 'graphql-http/lib/use/express';
import { buildSchema } from 'graphql';
import { typeDefs, rootResolvers, users, posts, comments } from '../schema.js';

// Build executable schema
const schema = buildSchema(typeDefs);

// Extended root resolvers for graphql-js style
const root = {
  ...rootResolvers,

  // Type resolvers need to be in root for buildSchema
  User: {
    posts: (user) => posts.filter(p => p.authorId === user.id),
    comments: (user) => comments.filter(c => c.authorId === user.id),
  },
};

const app = express();

// Health check
app.get('/health', (req, res) => res.send('ok'));

// GraphQL endpoint
app.all('/graphql', createHandler({
  schema,
  rootValue: root,
}));

const PORT = process.env.PORT || 4001;
app.listen(PORT, '0.0.0.0', () => {
  console.log(`graphql-js server running at http://localhost:${PORT}/graphql`);
});
