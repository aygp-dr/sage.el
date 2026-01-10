# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in sage.el, please report it responsibly.

### How to Report

1. **Do NOT open a public GitHub issue** for security vulnerabilities
2. Email the maintainer directly at: j@wal.sh
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Any suggested fixes (optional)

### What to Expect

- **Acknowledgment**: Within 48 hours
- **Initial Assessment**: Within 7 days
- **Fix Timeline**: Depends on severity
  - Critical: 24-72 hours
  - High: 1-2 weeks
  - Medium: 2-4 weeks
  - Low: Next release

### Security Considerations

#### API Keys

- Never commit API keys to the repository
- Use environment variables or `.env` files (gitignored)
- API keys are transmitted via HTTP headers, not URL parameters

#### Shell Commands

- All shell commands use `shell-quote-argument` to prevent injection
- User input is sanitized before passing to shell

#### File Operations

- File operations are restricted to the configured workspace
- Path traversal attempts are blocked

### Security Best Practices for Users

1. **Protect your API keys**
   - Use `.env` files with proper permissions (`chmod 600 .env`)
   - Never share API keys in issues or discussions

2. **Review tool outputs**
   - sage.el can execute shell commands via tools
   - Review command outputs before acting on them

3. **Use trusted models**
   - Only connect to trusted LLM providers
   - Verify Ollama models before use

## Acknowledgments

We thank security researchers who responsibly disclose vulnerabilities.
