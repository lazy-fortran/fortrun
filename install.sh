#!/bin/bash
# Install script for fortran CLI tool
# This script runs fpm install and copies configuration files

set -e

echo "Installing fortran CLI tool..."

# Run fpm install
fpm install "$@"

# Create config directory if it doesn't exist
CONFIG_DIR="$HOME/.config/fortran"
mkdir -p "$CONFIG_DIR"

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Copy registry files if they don't exist (to preserve user modifications)
if [ ! -f "$CONFIG_DIR/registry.toml" ]; then
    echo "Installing registry.toml to $CONFIG_DIR/"
    cp "$SCRIPT_DIR/registry.toml" "$CONFIG_DIR/"
else
    echo "registry.toml already exists at $CONFIG_DIR/, skipping..."
fi

if [ ! -f "$CONFIG_DIR/module_index.toml" ]; then
    echo "Installing module_index.toml to $CONFIG_DIR/"
    cp "$SCRIPT_DIR/module_index.toml" "$CONFIG_DIR/"
else
    echo "module_index.toml already exists at $CONFIG_DIR/, skipping..."
fi

echo "Installation complete!"
echo ""
echo "Configuration files installed to: $CONFIG_DIR/"
echo "You can customize the registry by editing:"
echo "  - $CONFIG_DIR/registry.toml"
echo "  - $CONFIG_DIR/module_index.toml"