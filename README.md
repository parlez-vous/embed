# [ParlezVous](https://parlezvous.io/) embeddable widget

This is repo represents the commenting widget for ParlezVous - the open and privacy-oriented commenting system.


Demo: https://demo.parlezvous.io/ (scroll down to see a working commenting widget)


## Running The App Locally

The embed is meant to be run as a sub-application on a host webpage.

To emulate this locally, you need to run the demo app located at `demo/index.html`.

```
> cd demo
> python -m http.server
```

Then once you have the demo running, it will begin fetching the actual embed app from localhost:3000

Additionally, you'll want to map localhost to `dev.parlezvous.io` by adding an entry in your `/etc/hosts` file:

```
127.0.0.1       dev.parlezvous.io
```

The mapping isn't really necessary, but it is useful for back-end purposes (The back-end will create a new record in the `posts` table according to the host url).

Then you can run the elm app:

```
npm start
```

Which will allow the host `index.html` file to load the file from port 3000 and include it inside of an iframe.

This is a work in progress. Read here for more info:

https://gdelgado.ca/parlez-vous-anglais-part-1.html#title

## TODO:
  - inline CSS
