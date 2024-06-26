// License: GNU GPL v3.0 (see `../license`)

'use strict';

const puppeteer = require('puppeteer');

const path = require('path');
const process = require('process');
const { execFileSync } = require('node:child_process');

// `argv` = `[node, ../imagemap.js, indexfoo.html]`
const htmlFile = path.join(process.cwd(), process.argv[2]);
const imageBasename = path.parse(htmlFile).name;

// why don't standard libraries have simple, yet powerful high-level functions
// like this?!
Number.prototype.noLessThan = function(x) {
  return Math.max(this, x);
}

const screenshotMap = async ({ width, height, isGrayscale }) => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.setViewport({ width, height });

  const getZoom = () => page.evaluate('map.getZoom()');
  const saveScreenshot = async () => {
    const zoom = await getZoom();
    const suffix = isGrayscale ? '_gray' : '';
    const file = `${imageBasename}_${zoom}${suffix}.png`;
    await page.screenshot({path: file});

    if (isGrayscale) {
      execFileSync('mogrify', ['-set', 'colorspace', 'Gray', file]);
    }

    console.log(`saved screenshot at zoom ${zoom} to ${file}`);
    return zoom;
  }

  const timeout = 2 * 60 * 1000;

  await page.goto(`file:${htmlFile}`, { waitUntil: 'networkidle0', timeout });
  const initialZoom = await saveScreenshot();

  const minZoom = 9;
  // enough zoomed out screenshots to end at minZoom
  const numScreenshots = (initialZoom - minZoom).noLessThan(4);

  for (const _ of Array.from(Array(numScreenshots).keys())) {
    await page.tap('[aria-label="Zoom out"]');
    await page.waitForNetworkIdle({ timeout });
    await saveScreenshot();
  };

  await browser.close();
};

// note: using these two commands w/o `await` (and the `async` wrapper)
// unintentionally makes the rendering process save two sets of screenshots in
// parallel! but some tile rendering may timeout due to the bigger amount of
// parallel requests, so it's disabled
(async () => {
  // PocketBook 740 Color's display b/w resolution, although color resolution is 3× less
  await screenshotMap({ width: 1872, height: 1404, isGrayscale: true });
  await screenshotMap({ width: 1872 / 3, height: 1404 / 3, isGrayscale: false });
})();
