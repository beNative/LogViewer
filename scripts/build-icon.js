const sharp = require('sharp');
const path = require('path');
const fs = require('fs');

const svgPath = path.resolve(__dirname, '../build/icon.svg');
const pngPath = path.resolve(__dirname, '../build/icon.png');

console.log(`Converting ${svgPath} to ${pngPath}...`);

sharp(svgPath)
    .resize(256, 256)
    .png()
    .toFile(pngPath)
    .then(info => {
        console.log('Icon generated successfully:', info);
    })
    .catch(err => {
        console.error('Error generating icon:', err);
        process.exit(1);
    });
