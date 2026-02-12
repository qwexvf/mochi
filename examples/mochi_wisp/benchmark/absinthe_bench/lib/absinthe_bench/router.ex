defmodule AbsintheBench.Router do
  use Plug.Router

  plug Plug.Parsers,
    parsers: [:json],
    pass: ["application/json"],
    json_decoder: Jason

  plug :match
  plug :dispatch

  forward "/graphql",
    to: Absinthe.Plug,
    init_opts: [schema: AbsintheBench.Schema]

  get "/health" do
    send_resp(conn, 200, "ok")
  end

  match _ do
    send_resp(conn, 404, "Not found")
  end
end
