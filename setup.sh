#!/bin/bash

# Setup script for ADS4All project

echo "🚀 Setting up ADS4All project..."

# Check if Stack is installed
if ! command -v stack &> /dev/null; then
    echo "❌ Stack is not installed. Please install Stack first:"
    echo "   curl -sSL https://get.haskellstack.org/ | sh"
    echo "   or"
    echo "   brew install haskell-stack (on macOS)"
    exit 1
fi

echo "✅ Stack is installed"

# Setup GHC and dependencies
echo "📦 Setting up GHC and dependencies..."
stack setup

# Build the project
echo "🔨 Building the project..."
stack build

# Run tests
echo "🧪 Running tests..."
stack test

# Generate documentation
echo "📚 Generating documentation..."
stack haddock

echo "✨ Setup complete!"
echo ""
echo "Next steps:"
echo "  • Run examples: stack run example-basic"
echo "  • Run benchmarks: stack bench"
echo "  • Open documentation: open .stack-work/dist/*/*/doc/html/ads4all/index.html"
echo "  • Push to GitHub: git remote add origin https://github.com/yourusername/ADS4All.git"
echo "                    git push -u origin main"