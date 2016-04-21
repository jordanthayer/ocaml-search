#!/bin/bash
for name in antic-grocery gtk_playground ocm rubiks tsp anticip histogram offline_fitting rucksack user_paths blfs image online search vacuum bounded_depth iterative_deepening pancake search_vis vis_nav csp job_shop parallel-search setcover wrmath learning partition sokoban wrthreads libplayerc_caml pdb structs wrutils dock_robot logistics plan-it synthetic_graph yapp dyn_robot luigi plots tiles experiments msa ps_plot topspin grid rdb tplan sliding_tiles
do
    echo $name
    cd $name
    clean_ocm.sh
    cd ..
done

exit 0
