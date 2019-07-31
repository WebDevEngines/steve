# Steve
Simple and scalable (YMMV) server to stream JSON document changes over SSE.

## Get Started
```
make run
```
## Server Configuration

### Environment Variables
Override the port you want the server to listen on and the timeout for SSE connections before re-connecting and retrieving a full document.
```
API_PORT=8093
API_IDLE_TIMEOUT_MS=600000
```
## HTTP API

#### Create or update document
Create or update a JSON document by POSTing to `/documents/:documentId` with the document's JSON body as the request body. Sending invalid JSON will result in a 400 being returned.

#### Listen for document changes
Listen for creation and subsequent changes to a document by GETing `/documents/:documentId/changes`. The initial connection to this endpoint will return the full document as the first SSE and the hold the connection open for subsequent updates. If the `documentId` is not found the connection will be held open until it is created.

## TODO
- [X] Cleanup document routes and get/set handlers
- [ ] Add authorization webhook
- [ ] Add client side example e.g. simple game
