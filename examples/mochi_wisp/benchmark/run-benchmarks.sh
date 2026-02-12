#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "Building and starting all servers..."
docker compose build

echo ""
echo "Starting servers..."
docker compose up -d mochi mercurius apollo yoga graphql-js

echo ""
echo "Waiting for servers to be ready..."
sleep 20

echo ""
echo "Running benchmarks..."
docker compose run --rm benchmark

echo ""
echo "Stopping servers..."
docker compose down

echo ""
echo "Done! Results are in ./results/"
ls -la ./results/
