let se = ReasonReact.stringToElement;

type todo = {
  id: int,
  title: string,
  completed: bool
};

module TodoItem = {
  let component = ReasonReact.statelessComponent "TodoItem";
  let make ::item ::onToggle _children => {
    ...component,
    render: fun _ =>
      <div className="item" onClick=(fun _evt => onToggle ())>
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean item.completed)
          readOnly=(Js.Boolean.to_js_boolean true)
        />
        (se item.title)
      </div>
  };
};

module Input = {
  type state = string;
  type action =
    | ChangeText string
    | KeyDown string;
  let component = ReasonReact.reducerComponent "Input";
  let eventValue event :string => (ReactDOMRe.domElementToObj (ReactEventRe.Form.target event))##value;
  let make ::onSubmit _children => {
    ...component,
    reducer: fun action text =>
      switch action {
      | ChangeText newText => ReasonReact.Update newText
      | KeyDown key =>
        switch key {
        | "Enter" => ReasonReact.UpdateWithSideEffects "" (fun _self => onSubmit text)
        | _ => ReasonReact.NoUpdate
        }
      },
    initialState: fun () => "",
    render: fun {state: text, reduce} =>
      <input
        value=text
        onKeyUp=(reduce (fun e => KeyDown (ReactEventRe.Keyboard.key e)))
        onChange=(reduce (fun e => ChangeText (eventValue e)))
      />
  };
};

type action =
  | AddItem string
  | ToggleItem int;

let lastId = ref 0;

let newItem title => {
  lastId := !lastId + 1;
  {title, completed: false, id: !lastId}
};

/* I've gone ahead and made a shortened name for converting strings to elements */
type a_state = {items: list todo};

let component = ReasonReact.reducerComponent "TodoAppYay";

let toggleItem id items =>
  List.map (fun item => item.id === id ? {...item, completed: not item.completed} : item) items;

let make _children => {
  ...component,
  initialState: fun () => {items: [{id: 0, title: "hey", completed: false}]},
  reducer: fun action state =>
    switch action {
    | AddItem text => ReasonReact.Update {items: [newItem text, ...state.items]}
    | ToggleItem id => ReasonReact.Update {items: toggleItem id state.items}
    },
  render: fun {reduce, state: {items}} =>
    <div className="app">
      <div className="title"> (se "What to do") </div>
      <Input onSubmit=(reduce (fun t => AddItem t)) />
      <div className="items">
        (
          ReasonReact.arrayToElement (
            Array.of_list (
              List.map
                (
                  fun item =>
                    <TodoItem
                      key=(string_of_int item.id)
                      onToggle=(reduce (fun _ => ToggleItem item.id))
                      item
                    />
                )
                items
            )
          )
        )
      </div>
      <div className="footer"> (se (string_of_int (List.length items) ^ " items")) </div>
    </div>
};
