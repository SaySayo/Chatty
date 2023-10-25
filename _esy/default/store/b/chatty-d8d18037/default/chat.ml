#1 "chat.eml.ml"
let home =
let ___eml_buffer = Buffer.create 4096 in
(Buffer.add_string ___eml_buffer "<html>\n<body>\n  <form>\n    <input type=\"submit\" value=\"Send\">\n    <input type=\"text\" id=\"message\" size=\"64\" autofocus>\n  </form>\n  <script>\n    let message = document.getElementById(\"message\");\n    let chat = document.querySelector(\"body\");\n    let socket = new WebSocket(\"ws://\" + window.location.host + \"/websocket\");\n\n    socket.onmessage = function (event) {\n      let item = document.createElement(\"div\");\n      item.innerText = event.data;\n      chat.appendChild(item);\n    };\n\n    document.querySelector(\"form\").onsubmit = function () {\n      if (socket.readyState != WebSocket.OPEN)\n        return false;\n      if (!message.value)\n        return false;\n\n      socket.send(message.value);\n      message.value = \"\";\n      return false;\n    };\n  </script>\n</body>\n</html>\n\n");
(Buffer.contents ___eml_buffer)
#33 "chat.eml.ml"
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
  let rec loop () =
    match%lwt Dream.receive client with
    | Some message ->
      let%lwt () = send message in
      loop ()
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
