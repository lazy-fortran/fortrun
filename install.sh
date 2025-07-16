#!/bin/bash
# Install script for fortran CLI tool
# This script runs fpm install and copies configuration files

set -e

echo "Installing fortran CLI tool..."

# Parse arguments to separate FPM flags from install options
FPM_FLAGS=""
INSTALL_ARGS=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --flag)
      # Combine all flags into a single quoted string
      FPM_FLAGS="$FPM_FLAGS --flag \"$2\""
      shift 2
      ;;
    --flag=*)
      FPM_FLAGS="$FPM_FLAGS $1"
      shift
      ;;
    --profile=*|--profile)
      # Handle profile flag for fpm
      if [[ $1 == --profile ]]; then
        FPM_FLAGS="$FPM_FLAGS --profile $2"
        shift 2
      else
        FPM_FLAGS="$FPM_FLAGS $1"
        shift
      fi
      ;;
    *)
      INSTALL_ARGS="$INSTALL_ARGS $1"
      shift
      ;;
  esac
done

# Run fpm install with appropriate flags
if [[ -n "$FPM_FLAGS" ]]; then
  echo "Building with flags: $FPM_FLAGS"
  eval "fpm build $FPM_FLAGS"
  echo "Installing binary..."
  eval "fpm install $INSTALL_ARGS"
else
  fpm install $INSTALL_ARGS
fi

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
