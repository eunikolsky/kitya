'use strict';

const puppeteer = require('puppeteer');

const path = require('path');

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.setViewport({
    width: 1680,
    height: 1050,
    deviceScaleFactor: 2,
  });
  //await page.setExtraHTTPHeaders({referer: 'https://www.openstreetmap.org/'});
  await page.goto(`file:${path.join(__dirname, 'map.html')}`, { waitUntil: 'networkidle0' });
  await page.screenshot({path: 'example@2x.png'});

  /*await page.tap('[aria-label="Zoom out"]');
  await page.waitForNetworkIdle();
  await page.screenshot({path: 'example_zoom@2x.png'});

  await page.tap('[aria-label="Zoom out"]');
  await page.waitForNetworkIdle();
  await page.screenshot({path: 'example_zoom2@2x.png'});*/

  await browser.close();
})();
