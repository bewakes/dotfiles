import json
from subprocess import call
from http.server import HTTPServer, BaseHTTPRequestHandler

HOMEPAGE = b'''
<html>
    <head>
        <title>Send key</title>
        <script>
            function send_request(key) {
                const url = '/';
                const body = JSON.stringify({'key': key})
                fetch(url, { method: "POST", body: body});
            }
        </script>
        <style>
            .big {
                width: 200px;
                height: 100px;
                font-size: 20px;
            }
        </style>
    </head>
    <body>
        <button class="big" id="left" type="button" onclick="send_request(this.id)">Left</button>
        <button class="big" id="space" type="button" onclick="send_request(this.id)">Space</button>
        <button class="big" id="Right" type="button" onclick="send_request(this.id)">Right</button>
        <br/>
        <h3>Workspaces</h3>
        <button class="big" id="w_1" type="button" onclick="send_request(this.id)">1</button>
        <button class="big" id="w_2" type="button" onclick="send_request(this.id)">2</button>
        <button class="big" id="w_3" type="button" onclick="send_request(this.id)">3</button>
        <button class="big" id="w_4" type="button" onclick="send_request(this.id)">4</button>
        <button class="big" id="w_5" type="button" onclick="send_request(this.id)">5</button> <button class="big" id="w_6" type="button" onclick="send_request(this.id)">6</button>
        <button class="big" id="w_7" type="button" onclick="send_request(this.id)">7</button>
        <button class="big" id="w_8" type="button" onclick="send_request(this.id)">8</button>
        <button class="big" id="w_9" type="button" onclick="send_request(this.id)">9</button>
        <br/>
        <h3>Volume</h3>
        <button class="big" id="v_+" type="button" onclick="send_request(this.id)">Vol +</button>
        <button class="big" id="v_-" type="button" onclick="send_request(this.id)">Vol -</button>
    </body>
</html>
'''


def xdotool_send(key):
    call(['xdotool', 'key', key])


class SimpleRequestHamdler(BaseHTTPRequestHandler):

    def do_GET(self):
        self.send_response(200)
        self.end_headers()
        self.wfile.write(HOMEPAGE)

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        body = self.rfile.read(content_length)
        data = json.loads(body)
        key: str = data.get('key', '')

        if key == 'space':
            xdotool_send('space')
        elif key == 'right':
            xdotool_send('Right')
        elif key == 'left':
            xdotool_send('Left')
        elif key.startswith('w_'):
            xdotool_send('Super+'+key[2:])
        elif key.startswith('v_'):
            incdec = key[2:]
            call(f'amixer -q sset Master 3%{incdec}'.split())
            call(['volume-notify'])
        else:
            print('no keys match')

        self.send_response(200)
        self.wfile.write(body)


httpd = HTTPServer(('0.0.0.0', 4433), SimpleRequestHamdler)

httpd.serve_forever()
