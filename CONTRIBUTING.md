# Contributing to ADS4All

Thank you for your interest in contributing to ADS4All! This document provides guidelines and instructions for contributing to the project.

## Code of Conduct

Please be respectful and constructive in all interactions. We aim to maintain a welcoming and inclusive environment for all contributors.

## How to Contribute

### Reporting Issues

1. Check if the issue already exists in the GitHub issue tracker
2. If not, create a new issue with:
   - Clear, descriptive title
   - Steps to reproduce (if it's a bug)
   - Expected vs actual behavior
   - System information (OS, GHC version, Stack version)

### Suggesting Features

1. Open an issue with the "enhancement" label
2. Describe the feature and its use case
3. Provide examples if possible

### Contributing Code

#### Setup

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/yourusername/ADS4All.git
   cd ADS4All
   ```
3. Add upstream remote:
   ```bash
   git remote add upstream https://github.com/originalrepo/ADS4All.git
   ```
4. Create a feature branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```

#### Development Workflow

1. Make your changes
2. Add tests for new functionality
3. Ensure all tests pass:
   ```bash
   stack test
   ```
4. Check code formatting:
   ```bash
   stack exec -- hlint src/ test/
   ```
5. Update documentation if needed
6. Commit with clear messages:
   ```bash
   git commit -m "Add: clear description of change"
   ```

#### Pull Request Process

1. Update your branch with latest upstream:
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```
2. Push to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```
3. Create a Pull Request with:
   - Clear title and description
   - Reference to related issues
   - Summary of changes
   - Test results

### Code Style Guidelines

- Use 2 spaces for indentation
- Keep lines under 100 characters
- Use meaningful variable and function names
- Add type signatures to all top-level functions
- Document exported functions with Haddock comments
- Follow existing code patterns in the project

### Testing Guidelines

- Write property-based tests using QuickCheck
- Include both positive and negative test cases
- Test edge cases
- Aim for high code coverage
- Performance tests should be added for critical paths

### Documentation

- Update README.md if adding new features
- Add Haddock comments to all exported functions
- Include usage examples in documentation
- Update CHANGELOG.md with your changes

## Project Structure

- `src/` - Source code
- `test/` - Test suites
- `examples/` - Example usage
- `benchmarks/` - Performance benchmarks
- `docs/` - Additional documentation

## Getting Help

- Open an issue for questions
- Check existing documentation
- Review similar PRs for examples

## Recognition

Contributors will be acknowledged in:
- The project README
- Release notes
- Git history

Thank you for contributing to ADS4All!