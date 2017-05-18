var server = null;

if(window.location.protocol === 'http:')
    server = "http://" + window.location.hostname + ":8088/janus";
else
    server = "https://" + window.location.hostname + ":8089/janus";

// janus object
var janus = null;
// streaming plugin object
var streaming = null;
var opaqueId = "streamingtest" + Janus.randomString(12);

var started = false;
var spinner = null;

var streamsList = null;
var selectedStream = null;

window.onload = function() {
    document.getElementById("server").innerHTML = "janus server address: " + server;

    Janus.init({debug: "all", callback: function() {
        // check if browser support webrtc
        if(!Janus.isWebrtcSupported()) {
				    alert("No WebRTC support... ");
				    return;
			  }
        janus = new Janus(
            {
                server: server,
                success: function() {
                    // attach to a streaming plugin
                    janus.attach(
                        {
                            plugin: "janus.plugin.streaming",
                            opaqueId: opaqueId,
                            success: function(pluginHandle) {
                                streaming = pluginHandle;
                                Janus.log("Streaming plugin attached! (" + streaming.getPlugin() + ", id=" + streaming.getId() + ")");
                                updateStreamsList();
                            },
                            error: function(error) {
                                alert("Error attaching streaming plugin... " + error);
                            },
                            onmessage: function(msg, jsep) {
                                Janus.debug(" ::: Got a message :::");
                                Janus.debug(JSON.stringify(msg));
                                var result = msg["result"];
                                if(result !== null && result !== undefined) {
										                if(result["status"] !== undefined && result["status"] !== null) {
											                  var status = result["status"];
											                  if(status === 'starting')
												                    $('#status').removeClass('hide').text("Starting, please wait...").show();
											                  else if(status === 'started')
												                    $('#status').removeClass('hide').text("Started").show();
											                  else if(status === 'stopped')
												                    stopStream();
										                }
									              } else if(msg["error"] !== undefined && msg["error"] !== null) {
										                bootbox.alert(msg["error"]);
										                stopStream();
										                return;
									              }
									              if(jsep !== undefined && jsep !== null) {
										                Janus.debug("Handling SDP as well...");
										                Janus.debug(jsep);
										                // Answer
										                streaming.createAnswer(
											                  {
												                    jsep: jsep,
												                    media: { audioSend: false, videoSend: false },	// We want recvonly audio/video
												                    success: function(jsep) {
													                      Janus.debug("Got SDP!");
													                      Janus.debug(jsep);
													                      var body = { "request": "start" };
													                      streaming.send({"message": body, "jsep": jsep});
													                      $('#watch').html("Stop").removeAttr('disabled').click(stopStream);
												                    },
												                    error: function(error) {
													                      Janus.error("WebRTC error:", error);
													                      bootbox.alert("WebRTC error... " + JSON.stringify(error));
												                    }
											                  });
									              }
                            },
                            onremotestream: function(stream) {
                                Janus.debug(" ::: Got a remote stream ::: ");
                                Janus.debug(JSON.stringify(stream));
                                Janus.attachMediaStream($('#remotevideo').get(0), stream);
                            }
                        });
                }
            }
        );
    }});
};

function updateStreamsList() {
    var body = { "request": "list" };
    streaming.send(
        {"message": body,
         success: function(result) {
             if(result === null || result == undefined) {
                 alert("Failed getting streaming list");
                 return;
             }
             if(result["list"] !== undefined && result["list"] !== null) {
                 var list = result["list"];
                 Janus.log("Got a list of available streams");
                 Janus.debug(list);
                 streamsList = list;
                 startStream();
             }
         }});
}

function startStream() {
    selectedStream = streamsList[0]["id"];
    Janus.log("Starting stream with ID " + selectedStream);
    var body = { "request": "watch", id: parseInt(selectedStream) };
    streaming.send({"message": body});
}
