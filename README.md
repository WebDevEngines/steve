# Steve
Simple SSE server

## Get Started
```
make run
```
## Server Configuration

### Environment Variables
```
API_PORT=8093
API_IDLE_TIMEOUT_MS=600000
HASH_MIN_LENGTH=16
MAX_PAYLOADS_STORED_PER_CHANNEL=1000
```

## API Endpoints

### /stream
/stream?channel=<channel_id>

#### <query-string> channel
The channel you want to receive events for.

#### <header> last-event-id (optional)
Pass this header to retrieve older events starting with the passed last-event-id.

### /broadcast
/broadcast?channel=<channel_id>&event=<event_id>&data=<data>

#### <query-string> channel
The broadcast this event to all clients listening on the channel.

#### <query-string> data
The data payload to send.

#### <query-string> event (optional)
The event within the channel you want to attach the data to. Note: this is optional. If not passed `message` will be used as the `event`.

## Usage


#### Client
```
<html>
<body>
  <script>
    var source = new EventSource("http://<host>:<port>/stream?channel=test");
    source.onmessage = function(event) {
      document.getElementById("result").innerHTML += event.data + "<br>";
    };
  </script>
  <div id="result">
  </div>
</body>
</html>
```

#### Server
```
curl "http://<host>:<port>/broadcast?channel=test&data=hello!"
```
