#!/bin/bash
set -e

echo "=== Starting build script ==="

# Install dependencies
echo "Installing yarn globally..."
npm i -g yarn

echo "Downloading linux-install.sh..."
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh

echo "Running linux-install.sh..."
./linux-install.sh

echo "Updating PATH..."
export PATH=/tmp/clojure/bin:$PATH

echo "Installing project dependencies with yarn..."
yarn install

# Build the app
echo "Building the app using shadow-cljs..."
npx shadow-cljs release app

# Copy README.md into public/app directory
echo "Copying README.md to public/app directory..."
cp README.md public/app

# Install Vercel Blob SDK
echo "Installing Vercel Blob SDK..."
npm install @vercel/blob

echo "=== Starting file upload process to Vercel Blob Storage ==="
node << 'EOF'
const { readFileSync } = require('fs');
const { join } = require('path');
const { put } = require('@vercel/blob');

// Define the directory where our built files live
const distPath = join(__dirname, 'public', 'app');
console.log("DEBUG: distPath set to", distPath);

// Files to be uploaded
const files = ['README.md', 'extension.js'];
console.log("DEBUG: Files to upload:", files);

// Retrieve environment variables or defaults
const resolvedBranch = process.env.VERCEL_GIT_COMMIT_REF || process.env.BRANCH || 'main';
const resolvedWorkspace = process.env.VERCEL_GIT_REPO_SLUG || process.env.WORKSPACE || 'myworkspace';
const token = process.env.BLOB_READ_WRITE_TOKEN || 'your_blob_token_here';
console.log("DEBUG: resolvedBranch =", resolvedBranch);
console.log("DEBUG: resolvedWorkspace =", resolvedWorkspace);
console.log("DEBUG: Blob token length =", token.length);

(async () => {
  for (const file of files) {
    const filePath = join(distPath, file);
    console.log(`DEBUG: Attempting to read file: ${filePath}`);
    let content;
    try {
      content = readFileSync(filePath);
      console.log(`DEBUG: Successfully read ${file} (${content.length} bytes)`);
    } catch (error) {
      console.error(`ERROR: Failed to read file ${file}:`, error);
      process.exit(1);
    }
    
    const pathname = resolvedBranch === 'main'
      ? `releases/${resolvedWorkspace}/${file}`
      : `releases/${resolvedWorkspace}/${resolvedBranch}/${file}`;
    console.log(`DEBUG: Computed pathname for ${file}: ${pathname}`);
    
    console.log(`Uploading ${file}...`);
    try {
      const blob = await put(pathname, content, {
        access: 'public',
        addRandomSuffix: false,
        token,
      });
      console.log(`✅ Uploaded ${file} to ${blob.url}`);
    } catch (error) {
      console.error(`ERROR: Upload failed for ${file}:`, error);
      process.exit(1);
    }
  }
  console.log("=== Deploy completed successfully! ===");
  const url = `https://discoursegraphs.com/releases/${resolvedWorkspace}/${resolvedBranch}`;
  console.log(`DEBUG: Endpoint URL: ${url}`);
})();
EOF

echo "=== Build script finished ==="
