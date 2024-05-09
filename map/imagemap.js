'use strict';

const puppeteer = require('puppeteer');

const path = require('path');
const process = require('process');

// `argv` = `[node, ../imagemap.js, indexfoo.html]`
const htmlFile = path.join(process.cwd(), process.argv[2]);
const imageBasename = path.parse(htmlFile).name;

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.setViewport({
    width: 1680,
    height: 1050,
    deviceScaleFactor: 2,
  });

  const getZoom = () => page.evaluate('map.getZoom()');
  const saveScreenshot = async () => await page.screenshot({path: `${imageBasename}_${await getZoom()}@2x.png`});

  await page.goto(`file:${htmlFile}`, { waitUntil: 'networkidle0' });
  await saveScreenshot();

  const numScreenshots = 5;

  // there was already one screenshot above
  for (const _ of Array.from(Array(numScreenshots - 1).keys())) {
    await page.tap('[aria-label="Zoom out"]');
    await page.waitForNetworkIdle();
    await saveScreenshot();
  };

  await browser.close();
})();
