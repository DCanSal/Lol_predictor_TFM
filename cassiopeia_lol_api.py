# -*- coding: utf-8 -*-
"""
Created on Wed Aug 19 16:26:08 2020

@author: DCanSal
"""

import cassiopeia as cass
import pandas as pd
from cassiopeia import Queue, Season

#Requieres de una clave verificada por RIOT 
cass.set_riot_api_key("ENTER_API_KEY")
cass.set_default_region("EUW")


#cargo las partidas
def maestria_campeon(participante, champion_name):
    champion_masteries = cass.get_champion_masteries(summoner = participante.summoner.name,
                                                     region = "EUW")
    if champion_masteries[champion_name] is None:
        return 0
    else:
        return champion_masteries[champion_name].points
    
def procesar_booleano(input_bool, list_to_append_value):
    if input_bool is True:
        list_to_append_value.append('1') #  1 == Victoria del equipo Azul
    else:
        list_to_append_value.append('0') 
def procesar_numerico(input_numerico, list_to_append_value):
    if input_numerico is None:
        list_to_append_value.append('0')
    else:
        list_to_append_value.append(input_numerico)

#ESTE ES EL ESQUEMA GENERAL 
def procesar_partida(id_partida):
    blue_wins         = list() #Lista principal 
    first_baron       = list()
    first_dragon      = list()
    first_blood       = list()
    first_inhibitor   = list()
    first_tower       = list()
    first_rifth       = list()
    tower_kills       = list()
    dragon_kills      = list()
    baron_kills       = list()
    inhibitor_kills   = list()
    rifth_kills       = list()
    picks_lista       = list()
    bans_lista        = list()
    maestrias_lista   = list()     
    partida           = cass.get_match(id_partida, region = "EUW")
    equipos           = partida.teams
    participantes     = partida.participants
    
    # TODO: GetOro(participantes, oro_lista)
    
    procesar_booleano(partida.blue_team.win                  , blue_wins      )
    procesar_booleano(partida.blue_team.first_baron          , first_baron    )
    procesar_booleano(partida.blue_team.first_dragon         , first_dragon   )
    procesar_booleano(partida.blue_team.first_blood          , first_blood    )
    procesar_booleano(partida.blue_team.first_inhibitor      , first_inhibitor)
    procesar_booleano(partida.blue_team.first_tower          , first_tower    )
    procesar_booleano(partida.blue_team.first_rift_herald    , first_rifth    )

    procesar_numerico(partida.blue_team.tower_kills          , tower_kills    )
    procesar_numerico(partida.blue_team.dragon_kills         , dragon_kills   )
    procesar_numerico(partida.blue_team.baron_kills          , baron_kills    )
    procesar_numerico(partida.blue_team.inhibitor_kills      , inhibitor_kills)
    procesar_numerico(partida.blue_team.rift_herald_kills    , rifth_kills    )
        
    for team in equipos:
        for ban in team.bans:
            if ban is None:
                bans_lista.append('0')
            else:
                bans_lista.append(ban.id)
    for participante in participantes:
        picks_lista.append(participante.champion.id)
        maestria  = maestria_campeon(participante,participante.champion.name)
        maestrias_lista.append(maestria)
    partida_procesada = blue_wins + first_dragon + first_baron + first_blood + first_inhibitor + first_tower + first_rifth + tower_kills + dragon_kills + baron_kills + inhibitor_kills + rifth_kills + picks_lista +bans_lista +maestrias_lista
    return(partida_procesada)

   
lista_ids = pd.read_csv("lista_ids", header = None)
partidas_procesadas = list()
contando = 1
for id_partida in lista_ids[0][56160:]:
    print("{parcial}/{total}".format(parcial=contando, total=len(lista_ids)))
    try:
        partida_procesada = procesar_partida(int(id_partida))
        partidas_procesadas.append(partida_procesada)
    except Exception:
        pass        
    #SaveCSV(contador, partida_procesada)
    contando = contando + 1




with open ('partidas_batch4.csv','w') as file:
        for partida in partidas_procesadas:
            numPartidas = len(partida)
            contador = 0
            for elemento_partida in partida:
                if contador < numPartidas-1:
                    file.write("%u," % int(elemento_partida))
                else:
                    file.write("%u" % int(elemento_partida))
                contador = contador + 1
            file.write("\n")
        file.close()






historias=list()
with open('listamasters.txt', 'w', encoding = 'utf-16') as file:
    for item in jugadores_master:
        file.write("%s\n" % item)
    file.close()
    
with open('lista_ids', 'w', encoding = 'utf-8') as file:
    for item in historias:
        file.write("%u\n" % item)
    file.close()
#Aqui hacemos una llamada al servidor pidiendo la lista de jugadores en la 
#liga masters.
masters = cass.get_master_league(queue=Queue.ranked_solo_fives, region="EUW")
jugadores_master=list()

contador = 1
for master in masters:
    print("{parcial}/{total} - {summoner}".format(parcial=contador, total=len(masters), summoner=master.summoner.name))
    jugadores_master.append(master.summoner.name)
    
    summoner      = cass.get_summoner(name = master.summoner.name, region="EUW")
    match_history = summoner.match_history(seasons={Season.season_9}, queues={Queue.ranked_solo_fives})
    #match_history(seasons={Season.season_9}, queues={Queue.ranked_solo_fives})
        
    
    for historia in match_history:
        print(historia)
        historias.append(historia.id)
        
    contador = contador + 1

# TODO: historias aqui es una vble. local pero bien podriamos ir leyendolas 
#desde CSVs
partidas_procesadas =list()
contador = 1
for id_partida in historias[25160:]:
    print("{parcial}/{total}".format(parcial=contador, total=len(historias)))
    partida_procesada = procesar_partida(id_partida)
    partidas_procesadas.append(partida_procesada)
        
    #SaveCSV(contador, partida_procesada)
    contador = contador + 1
    
    
    
