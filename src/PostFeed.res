let s = React.string

open Belt

type state = {posts: array<Post.t>, forDeletion: Map.String.t<Js.Global.timeoutId>}

type action =
  | DeleteLater(Post.t, Js.Global.timeoutId)
  | DeleteAbort(Post.t)
  | DeleteNow(Post.t)

let reducer = (state, action) =>
  switch action {
  | DeleteLater(post, timeoutId) => {
      ...state,
      forDeletion: state.forDeletion->Map.String.set(post.id, timeoutId),
    }
  | DeleteAbort(post) => {...state, forDeletion: state.forDeletion->Map.String.remove(post.id)}
  | DeleteNow(post) => {
      posts: state.posts->Js.Array2.filter(p => p.id != post.id),
      forDeletion: state.forDeletion->Map.String.remove(post.id),
    }
  }

let toKey = (id, index) => id ++ Belt.Int.toString(index)

let postText = (post: Post.t) => {
  Belt.Array.mapWithIndex(post.text, (index, line) =>
    <p key={toKey(post.id, index)} className="mb-1 text-sm"> {s(line)} </p>
  )->React.array
}

module PostBlock = {
  @react.component
  let make = (~post: Post.t, ~dispatch, ~state) => {
    let (showPost, setshowPost) = React.useState(() => true)

    let showDelBlock = _ => {
      dispatch(DeleteLater(post, Js.Global.setTimeout(() => dispatch(DeleteNow(post)), 10000)))
      setshowPost(_ => false)
    }

    let restorePost = _ => {
      let timeoutId = state.forDeletion->Map.String.get(post.id)
      switch timeoutId {
      | Some(id) => {
          Js.Global.clearTimeout(id)
          dispatch(DeleteAbort(post))
        }
      | None => ()
      }
      setshowPost(_ => true)
    }

    let deleteImmediate = _ => {
      let timeoutId = state.forDeletion->Map.String.get(post.id)
      switch timeoutId {
      | Some(id) => {
          Js.Global.clearTimeout(id)
          dispatch(DeleteNow(post))
        }
      | None => ()
      }
      setshowPost(_ => false)
    }

    if showPost {
      <div className="max-w-3xl mx-auto mt-8 relative">
        <div
          className="bg-green-700 hover:bg-green-900 text-gray-300 hover:text-gray-100 px-8 py-4 mb-4">
          <h2 className="text-2xl mb-1"> {s(post.title)} </h2>
          <h3 className="mb-4"> {s(post.author)} </h3>
          {post->postText}
          <button
            onClick={showDelBlock}
            className="mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4">
            {s("Remove this post")}
          </button>
        </div>
      </div>
    } else {
      <div className="max-w-3xl mx-auto mt-8 relative bg-yellow-100 px-8 py-4 mb-4 h-40">
        <p className="text-center white mb-1">
          {s(
            `This post from ${post.title} by ${post.author} will be permanently removed in 10 seconds.`,
          )}
        </p>
        <div className="flex justify-center">
          <button
            onClick={restorePost}
            className="mr-4 mt-4 bg-yellow-500 hover:bg-yellow-900 text-white py-2 px-4">
            {s("Restore")}
          </button>
          <button
            onClick={deleteImmediate}
            className="mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4">
            {s("Delete Immediately")}
          </button>
        </div>
        <div className="bg-red-500 h-2 w-full absolute top-0 left-0 progress" />
      </div>
    }
  }
}

let initialState = {posts: Post.examples, forDeletion: Map.String.empty}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState)

  Array.map(state.posts, post => <PostBlock key=post.id post dispatch state />)->React.array
}
