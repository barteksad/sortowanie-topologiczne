(* autor : Bartek Sadlej *)


open PMap

exception Cykliczne;;


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
    
    let rec time_setter results time visited event=
        
        if not (mem event results) then

            if mem event visited then
                raise Cykliczne
            else

            let (updated_results,updated_time,updated_visited) =

            try 
                let next_events = find event adjacency_map in

                 List.fold_left 
                    
                    (fun (updated_results , updated_time , updated_visited) next_event ->
                        time_setter 
                            updated_results 
                            updated_time 
                            updated_visited 
                            next_event) 

                    (results,time,add event 1 visited) 

                    next_events
            with
                Not_found -> (results,time,add event 1 visited) in
                
            (add event updated_time updated_results,updated_time+1,updated_visited)

        else
            (results,time,visited) in
    
    let (result_temp,time_end,_) = 
    List.fold_left 
        
        (fun (results,time,visited) (event,_) ->
            time_setter 
                results 
                time 
                visited 
                event) 

        (empty,1,empty) 
        adjacency_list in
    
    (foldi (fun event time acc -> (time_end - (List.hd time),event)::acc) result_temp []) |> List.sort compare |> List.map snd;;

