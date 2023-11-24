let home =
  <html>
  <body>
    <form id="username-form">
      <span>Write your username: </span>
      <input id="username" type="text">
      <input type="submit" value="Send">
    </form>
    <form id="message-form">
      <input type="submit" value="Send">
      <input type="text" id="message" size="64" autofocus>
    </form>
    <script>
      let message = document.getElementById("message");
      let chat = document.querySelector("body");
      let socket = new WebSocket("ws://" + window.location.host + "/websocket");
      let username = document.getElementById("username");

      socket.onmessage = function (event) {
        let item = document.createElement("div");
        item.innerText = event.data;
        chat.appendChild(item);
      };

      document.getElementById("username-form").onsubmit = function () {
        if (socket.readyState != WebSocket.OPEN)
          return false;
        if (!username.value)
          return false;

        socket.send("usr: " + username.value);
        return false;
      };

      document.getElementById("message-form").onsubmit = function () {
        if (socket.readyState != WebSocket.OPEN)
          return false;
        if (!message.value)
          return false;

        socket.send("msg: " + message.value);
        message.value = "";
        return false;
      };
    </script>
  </body>
  </html>

let clients : (int, Dream.websocket) Hashtbl.t =
  Hashtbl.create 5

let track =
  let last_client_id = ref 0 in
  fun websocket ->
    last_client_id := !last_client_id + 1;
    Hashtbl.replace clients !last_client_id websocket;
    !last_client_id

let forget client_id =
  Hashtbl.remove clients client_id

let send message =
  Hashtbl.to_seq_values clients
  |> List.of_seq
  |> Lwt_list.iter_p (fun client -> Dream.send client message)

let handle_client client =
  let client_id = track client in
  let username = ref "anonymous" in
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      (match String.split_on_char ':' message with 
      | ["usr"; name] -> username := name;
      loop ()
      | ["msg"; message] -> 
        let%lwt () = send (!username ^ ": " ^ message) in
        loop ()
      | _ -> Dream.log "No username or message detected";
        loop ()
      ) 
    | None ->
      forget client_id;
      Dream.close_websocket client
  in
  loop ()

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.get "/"
      (fun _ -> Dream.html home);

    Dream.get "/websocket"
      (fun _ -> Dream.websocket handle_client);

  ]
