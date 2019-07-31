# Steve
Simple and scalable (YMMV) server to stream JSON document changes over SSE.

## Get Started
```
make run
```
or
```
docker build . -t steve
docker run -p 0.0.0.0:8093:8093 -it steve
```

## Server Configuration

### Environment Variables
Override the port you want the server to listen on and the timeout for SSE connections before re-connecting and retrieving a full document.
```
API_PORT=8093
API_IDLE_TIMEOUT_MS=600000
API_NUM_PROCS=5
AUTHORIZATION_WEBHOOK=<your_auth_service_url>
```
## HTTP(S) API

#### Create or update document
Create or update a JSON document by POSTing to `/documents/:documentId` with the document's JSON body as the request body. Sending invalid JSON will result in a 400 being returned.

#### Listen for document changes
Listen for creation and subsequent changes to a document by GETing `/documents/:documentId/changes`. The initial connection to this endpoint will return the full document as the first SSE and the hold the connection open for subsequent updates. If the `documentId` is not found the connection will be held open until it is created.

## Authorization Webhook
Access to documents can be restricted by POSTing to an external authorization service with the passed `Authorization` header value and `documentId` in the request body i.e.
```
{
  "authorization": "the header value",
  "documentId": "the document id"
}
```
 If a `200` is returned from the external service then the request will be allowed otherwise a `401` will be returned. The header is expected to be in the format `Authorization: Bearer <token>`.

## Erlang API

#### List number of open connections
To list the number of open streaming connections connect to the beam process and run `steve_channel:num_streams()`.

## TODO
- [X] Cleanup document routes and get/set handlers
- [X] Add authorization webhook
- [ ] Improved list diffing
- [ ] Add client side example e.g. simple game
