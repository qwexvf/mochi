// schema.js
// Shared GraphQL schema and data for all benchmark servers
// Matches the complex schema used in Mochi benchmarks

// ============================================================================
// Sample Data (matching Mochi's complex_schema.gleam)
// ============================================================================

export const users = [
  { id: '1', name: 'Alice', email: 'alice@example.com', role: 'ADMIN', createdAt: '2024-01-01T00:00:00Z' },
  { id: '2', name: 'Bob', email: 'bob@example.com', role: 'MEMBER', createdAt: '2024-01-02T00:00:00Z' },
  { id: '3', name: 'Charlie', email: 'charlie@example.com', role: 'GUEST', createdAt: '2024-01-03T00:00:00Z' },
  { id: '4', name: 'Diana', email: 'diana@example.com', role: 'MEMBER', createdAt: '2024-01-04T00:00:00Z' },
  { id: '5', name: 'Eve', email: 'eve@example.com', role: 'ADMIN', createdAt: '2024-01-05T00:00:00Z' },
];

export const posts = [
  { id: '1', title: 'Hello World', content: 'My first post', authorId: '1', createdAt: '2024-02-01T00:00:00Z', status: 'PUBLISHED' },
  { id: '2', title: 'GraphQL Tips', content: 'How to use GraphQL effectively', authorId: '1', createdAt: '2024-02-02T00:00:00Z', status: 'PUBLISHED' },
  { id: '3', title: 'Gleam is Great', content: 'Why I love Gleam', authorId: '2', createdAt: '2024-02-03T00:00:00Z', status: 'PUBLISHED' },
  { id: '4', title: 'Draft Post', content: 'Work in progress', authorId: '3', createdAt: '2024-02-04T00:00:00Z', status: 'DRAFT' },
  { id: '5', title: 'BEAM VM', content: 'Exploring Erlang VM', authorId: '4', createdAt: '2024-02-05T00:00:00Z', status: 'PUBLISHED' },
];

export const comments = [
  { id: '1', content: 'Great post!', authorId: '2', postId: '1', createdAt: '2024-02-01T12:00:00Z' },
  { id: '2', content: 'Thanks for sharing', authorId: '3', postId: '1', createdAt: '2024-02-01T13:00:00Z' },
  { id: '3', content: 'Very helpful', authorId: '1', postId: '3', createdAt: '2024-02-03T14:00:00Z' },
  { id: '4', content: 'I agree!', authorId: '4', postId: '2', createdAt: '2024-02-02T15:00:00Z' },
  { id: '5', content: 'Nice insights', authorId: '5', postId: '5', createdAt: '2024-02-05T16:00:00Z' },
];

// ============================================================================
// GraphQL SDL Schema
// ============================================================================

export const typeDefs = `
  enum Role {
    ADMIN
    MEMBER
    GUEST
  }

  enum PostStatus {
    DRAFT
    PUBLISHED
    ARCHIVED
  }

  type User {
    id: ID!
    name: String!
    email: String!
    role: Role!
    createdAt: String!
    posts: [Post!]!
    comments: [Comment!]!
  }

  type Post {
    id: ID!
    title: String!
    content: String!
    author: User!
    status: PostStatus!
    createdAt: String!
    comments: [Comment!]!
  }

  type Comment {
    id: ID!
    content: String!
    author: User!
    post: Post!
    createdAt: String!
  }

  type UserConnection {
    nodes: [User!]!
    totalCount: Int!
  }

  type PostConnection {
    nodes: [Post!]!
    totalCount: Int!
  }

  input CreateUserInput {
    name: String!
    email: String!
    role: Role!
  }

  input CreatePostInput {
    title: String!
    content: String!
    authorId: ID!
  }

  type Query {
    # User queries
    users: [User!]!
    user(id: ID!): User
    usersByRole(role: Role!): [User!]!

    # Post queries
    posts: [Post!]!
    post(id: ID!): Post
    postsByStatus(status: PostStatus!): [Post!]!

    # Connection queries (pagination)
    usersConnection(first: Int, offset: Int): UserConnection!
    postsConnection(first: Int, offset: Int): PostConnection!

    # Search
    search(term: String!): [Post!]!
  }

  type Mutation {
    createUser(input: CreateUserInput!): User!
    createPost(input: CreatePostInput!): Post!
    deletePost(id: ID!): Boolean!
  }

  type Subscription {
    postCreated: Post!
    commentAdded(postId: ID!): Comment!
  }
`;

// ============================================================================
// Resolvers
// ============================================================================

export const resolvers = {
  Query: {
    users: () => users,
    user: (_, { id }) => users.find(u => u.id === id),
    usersByRole: (_, { role }) => users.filter(u => u.role === role),

    posts: () => posts,
    post: (_, { id }) => posts.find(p => p.id === id),
    postsByStatus: (_, { status }) => posts.filter(p => p.status === status),

    usersConnection: (_, { first = 10, offset = 0 }) => ({
      nodes: users.slice(offset, offset + first),
      totalCount: users.length,
    }),

    postsConnection: (_, { first = 10, offset = 0 }) => ({
      nodes: posts.slice(offset, offset + first),
      totalCount: posts.length,
    }),

    search: (_, { term }) => posts.filter(p =>
      p.title.toLowerCase().includes(term.toLowerCase()) ||
      p.content.toLowerCase().includes(term.toLowerCase())
    ),
  },

  Mutation: {
    createUser: (_, { input }) => {
      const newUser = {
        id: String(users.length + 1),
        ...input,
        createdAt: new Date().toISOString(),
      };
      users.push(newUser);
      return newUser;
    },

    createPost: (_, { input }) => {
      const newPost = {
        id: String(posts.length + 1),
        ...input,
        status: 'DRAFT',
        createdAt: new Date().toISOString(),
      };
      posts.push(newPost);
      return newPost;
    },

    deletePost: (_, { id }) => {
      const index = posts.findIndex(p => p.id === id);
      if (index >= 0) {
        posts.splice(index, 1);
        return true;
      }
      return false;
    },
  },

  User: {
    posts: (user) => posts.filter(p => p.authorId === user.id),
    comments: (user) => comments.filter(c => c.authorId === user.id),
  },

  Post: {
    author: (post) => users.find(u => u.id === post.authorId),
    comments: (post) => comments.filter(c => c.postId === post.id),
  },

  Comment: {
    author: (comment) => users.find(u => u.id === comment.authorId),
    post: (comment) => posts.find(p => p.id === comment.postId),
  },
};

// For graphql-js which uses root resolvers
export const rootResolvers = {
  ...resolvers.Query,
  ...resolvers.Mutation,
};
