#!/bin/bash
set -e

# Function to filter logs
filter_logs() {
  grep -v "^Downloading: " | grep -v "^Downloaded: "
}

# Start logging with filter
exec > >(filter_logs | tee build-log.txt) 2>&1

echo "=== Starting build script ==="

# Install dependencies
echo "Installing yarn globally..."
npm i -g yarn

curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh

./linux-install.sh

export PATH=/tmp/clojure/bin:$PATH

echo "Installing project dependencies with yarn..."
yarn install

# Build the app
npx shadow-cljs release app

# Copy README.md into public/app directory
cp README.md public/app
 
# Install Vercel Blob SDK
npm install @vercel/blob


# Save the Node.js script to a file
cat > upload-files.js << 'EOF'
const { readFileSync } = require('fs');
const { join } = require('path');
const { put } = require('@vercel/blob');

// Define the directory where our built files live
const distPath = join(__dirname, 'public', 'app');

// Files to be uploaded
const files = ['README.md', 'extension.js'];

// Retrieve environment variables or defaults
const resolvedBranch = process.env.VERCEL_GIT_COMMIT_REF || process.env.BRANCH || 'main';
const resolvedWorkspace = process.env.VERCEL_GIT_REPO_SLUG || process.env.WORKSPACE || 'myworkspace';
const token = process.env.BLOB_READ_WRITE_TOKEN || 'your_blob_token_here';

(async () => {
  for (const file of files) {
    const filePath = join(distPath, file);
    console.log(`DEBUG: Attempting to read file: ${filePath}`);
    let content;
    try {
      content = readFileSync(filePath);
    } catch (error) {
      console.error(`ERROR: Failed to read file ${file}:`, error);
      process.exit(1);
    }
    
    const pathname = resolvedBranch === 'main'
      ? `releases/${resolvedWorkspace}/${file}`
      : `releases/${resolvedWorkspace}/${resolvedBranch}/${file}`;
    
    try {
      const blob = await put(pathname, content, {
        access: 'public',
        addRandomSuffix: false,
        token,
      });
    } catch (error) {
      console.error(`ERROR: Upload failed for ${file}:`, error);
      process.exit(1);
    }
  }
  console.log("=== Deploy completed successfully! ===");
  const url = resolvedBranch === 'main'
    ? `https://discoursegraphs.com/releases/${resolvedWorkspace}`
    : `https://discoursegraphs.com/releases/${resolvedWorkspace}/${resolvedBranch}`;
  console.log(`✅ DEBUG: Endpoint URL: ${url}`);
})();
EOF

# Run the upload script
node upload-files.js

echo "=== Build script finished ==="

# Optional: Display important build artifacts
echo "=== Build Summary ==="
echo "Files in public/app directory:"
ls -la public/app