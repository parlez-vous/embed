const { readFileSync, renameSync, writeFileSync } = require('fs')
const cheerio = require('cheerio')

const htmlFileName = './dist/index.html'

const htmlFile = readFileSync(htmlFileName, { encoding: 'utf8' })
const appJsFile = readFileSync('./dist/app.js', { encoding: 'utf8' })

const $ = cheerio.load(htmlFile)

$('script')
  .removeAttr('src')
  .text(appJsFile)

const merged = $.html()

writeFileSync('./dist/iframe-app.html', merged, { encoding: 'utf8' })

renameSync(htmlFileName, './dist/original.html')

