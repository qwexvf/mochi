defmodule AbsintheBench.Schema do
  use Absinthe.Schema

  # Sample data (same as Mochi benchmark)
  @users [
    %{id: "1", name: "Alice", email: "alice@example.com", role: :admin},
    %{id: "2", name: "Bob", email: "bob@example.com", role: :member},
    %{id: "3", name: "Charlie", email: "charlie@example.com", role: :guest}
  ]

  enum :role do
    value :admin, as: "ADMIN"
    value :member, as: "MEMBER"
    value :guest, as: "GUEST"
  end

  object :user do
    field :id, non_null(:id)
    field :name, non_null(:string)
    field :email, non_null(:string)
    field :role, non_null(:role)
  end

  query do
    field :users, non_null(list_of(non_null(:user))) do
      resolve fn _, _ ->
        {:ok, @users}
      end
    end

    field :user, :user do
      arg :id, non_null(:id)

      resolve fn %{id: id}, _ ->
        user = Enum.find(@users, fn u -> u.id == id end)
        {:ok, user}
      end
    end
  end
end
