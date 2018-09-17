populate_dropdowns = function(monster_list)
{
    "use strict"
    let selects = document.getElementsByTagName("select");
    let monsters = Object.keys(monster_list).sort();

    for (let idx= 0; idx < selects.length; idx++)
    {
        let select = selects[idx];
        let newOption = document.createElement("option");
        newOption.text = '';
        select.add(newOption)

        for (let j in monsters)
        {
            newOption = document.createElement("option");
            newOption.text = monsters[j];
            select.add(newOption);
        }
    }
}
load_monster_png = function(div_id, new_monster)
{
    if(new_monster.length > 0)
    {
        document.getElementById(div_id).innerHTML = monster_list[new_monster];
    }
}