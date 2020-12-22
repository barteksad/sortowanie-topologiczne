(* autor : Bartek Sadlej *)


open PMap

exception Cykliczne


let topol (adjacency_list: ('a * 'a list) list) =
    (* dodawanie następników do mapy *)
    let adjacency_map = List.fold_left (
        fun part_result (this,neighbours) ->
            List.fold_left(
                fun temp_acc neighbour ->
                    add this neighbour temp_acc
            )
            part_result
            neighbours
    )
    empty
    adjacency_list in
    
    let rec time_setter event time starting_event results=
        if event = starting_event then
            raise Cykliczne
        else
        if not (mem event results) then 
            let (updated_time,updated_results) = List.fold_left (
                fun (updated_time,updated_results) next_event ->
                    time_setter next_event updated_time starting_event updated_results
                ) 
                (time,results) 
                (find event adjacency_map) in
        
        updated_time+1, add updated_time event updated_results
        else
            time,results in
    
    

