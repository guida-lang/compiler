npx esbuild bin/index.js  --bundle --outfile=build.cjs --format=cjs --platform=node

# Single executable applications (https://nodejs.org/api/single-executable-applications.html)
# This feature allows the distribution of a Node.js application conveniently to a system that does not have Node.js installed.

# 3. Generate the blob to be injected

node --experimental-sea-config sea-config.json

# 4. Create a copy of the node executable and name it according to your needs

cp $(command -v node) guida

# node -e "require('fs').copyFileSync(process.execPath, 'hello.exe')" # On windows

# 5. Remove the signature of the binary (macOS and Windows only)

codesign --remove-signature guida # On mac

# signtool remove /s guida.exe # On Windows (optional)

# 6. Inject the blob into the copied binary by running postject with the following options

# Linux
# npx postject guida NODE_SEA_BLOB sea-prep.blob \
#     --sentinel-fuse NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2

# On Windows - PowerShell
# npx postject guida.exe NODE_SEA_BLOB sea-prep.blob `
#     --sentinel-fuse NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2

# On Windows - Command Prompt
# npx postject guida.exe NODE_SEA_BLOB sea-prep.blob ^
#     --sentinel-fuse NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2 

# On macOS
npx postject guida NODE_SEA_BLOB sea-prep.blob \
    --sentinel-fuse NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2 \
    --macho-segment-name NODE_SEA

# 7. Sign the binary (macOS and Windows only)

# On macOS
codesign --sign - guida

# On Windows (optional)
# A certificate needs to be present for this to work. However, the unsigned binary would still be runnable.
# signtool sign /fd SHA256 guida.exe
