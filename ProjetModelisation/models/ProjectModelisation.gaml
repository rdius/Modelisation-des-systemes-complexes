/**
 *  Project Modelisation groupe 7
 *  Author: KAFANDO & KOUAME
 *  Description: Dans ce projet, nous simmulons un cas de sécours de personnes ayant subies des accidents de
 *  			circulation routiere.
 */

model Project

global {
	
	file bounds_shapefile <- file("../includes/bounds.shp");
	file buildings_shapefile <- file("../includes/buildings.shp");
	file hospitals_shapefile <- file("../includes/hospitals.shp");
	file roads_shapefile <- file("../includes/roads.shp");
	geometry shape <- envelope(bounds_shapefile);
	
	
	int nb_infirmiers <- int(nb_population/10) ;

	
	int nb_population parameter: 'nb_population' <- 100 min: 5 max:100;
	int nb_cas_accidents parameter: 'nb_cas_accidents' <- 10 min: 5 max:50;
	int max_ambulance parameter: 'nb_ambulance' <- 10 min:1  max:10;
		
	
	graph the_graph;
	
	// parametres de sortie
	int nb_urgence <- nb_cas_accidents update: personne count (each.has_accident);
    int nb_de_mort <- 0 update: personne count (each.est_mort);
    int nb_de_survivant <- 0 update: personne count (each.a_survecu);
    
    float taux_sauvtage update: nb_de_survivant/nb_urgence;
    float taux_non_sauve update: nb_de_mort/nb_urgence;
	
	init{
		// creation des hopitaux
		create centre_urgence from: hospitals_shapefile;
		// creation des routes
		create road from: roads_shapefile;
		the_graph <- as_edge_graph(road);
		// creation de la population
		create personne number:nb_population{
			//home <- one_of(building);
			//location <- any_location_in(home);
		}
		//create vehicule number: nb_vehicules;
		
		// affectation de certaines personnenes dans les hopitaux
		ask (nb_infirmiers among (list(personne))){
			hosp <- one_of(centre_urgence );
			location <- any_location_in(hosp);
		}
		// affectation de certaines personnenes sur les routes
		ask (nb_population among (list(personne))){
			roads <- one_of(road);
			location <- any_location_in(roads);
		}
		// creation des personnenes en urgence
		ask (nb_cas_accidents among (list(personne))) {
			set has_accident <- true;
			set energy <- rnd(100.0)+2.0;
			do change_etat;
		}
	}



    reflex stop when:(nb_urgence=0) {
        do halt;
    }

}

// species Position
species Position{
	int x;
	int y;
}
// species Person
species personne parent:Position{

	
	rgb color <- #green;
	float size <- 5.0;
	bool has_accident <- false;
	bool est_mort <- false;
	bool a_survecu <- false;
	float rayon_emission <- 500.0;
	float energy <- 50.0;
	float max_survi <- 150.0;
	
	// parametres pour disposer aleatoirement la population lors de la simulation
	//building home <- nil;
	centre_urgence hosp <- nil;
	road roads <- nil;
	
	list<centre_urgence> list_hosp <- nil;
	
	// action de changement de couleur
	action change_etat{
		if(has_accident and !est_mort){
			set color <- #red;
			do send_signal;
		}else if(has_accident and est_mort){
			set color <- #black;
		}else if(!has_accident){
			set color <- #green;
		}
	}
	//action d'envoi de signal en cas d'urgence aux hopitaux
	action send_signal{
		if(has_accident and energy < max_survi){
			list_hosp <- (centre_urgence at_distance(rayon_emission));
		}
		//write("le nombre d'hopitaux: "+length(list_hosp));
	}
	// reflex d'augmentation/reduction d'energie: gestion de survi ou de mort 
	reflex gestion_energy{
		if(hosp != nil and has_accident){	
			if(energy < max_survi){
				set energy <- energy  +1;
			}
		}else if(hosp = nil and has_accident and !est_mort){
			set energy <- energy -0.1;
		}
		if(energy <= 0.0 and has_accident and !est_mort){
			set est_mort <- true;
			nb_urgence <-nb_urgence-personne count (each.est_mort);
			do change_etat;
		}else if(energy >= max_survi and has_accident){
			//set has_accident <- false;
			set a_survecu <- true;
			//nb_urgence <-nb_urgence-1;
			//do change_etat;
			set color <- rgb('blue');
		}
		//nb_urgence <-nb_urgence-1;
	}
	
	
	
	
	// aspect de l'agent
	aspect base{
		draw circle(size) color:color;
	}
}
// species Hospitals
species centre_urgence {
	
	int nb_amb_used <- 0;
	int max_ambulance <- rnd(10);
	float radius_percep <- 600.0;
	float radius_percep_hosp <- 20.0;
	float rayon_emission <- 400.0;
	int max_emergency <- 50;
	int nb_emergency <- 0;
	int interval_time;
	bool is_sature <- false;
	bool amb_needed <- false;
	bool amb_to_other <- false;
	
	
	list<personne> list_accident;
	list<centre_urgence> list_hosp;
	list<centre_urgence> list_hosp_for_help;
	
	// reflex de reception de signal d'un patient
	reflex receive_p_urgence_signal{
		
		list_accident <- (personne at_distance(radius_percep)) where(each.has_accident);
		
		if(nb_emergency < max_emergency){
			if (nb_urgence>=0){
			do send_ambulance(self);
		}
		}
	}
	// reflex de reception de signal d'un autre hopital
	reflex receive_hop_urgence_signal{
		
		list_hosp_for_help <- (centre_urgence at_distance(radius_percep_hosp)) where(each.amb_needed);
		set amb_to_other <- true;
		if(nb_amb_used < max_ambulance){
			loop hospit over:list_hosp_for_help{
				if (nb_urgence>=0){
				do send_ambulance(hospit);
			}
			
			}
		}
	}
	
	
	// action d'envoi de signal aux autres hopitaux
	action send_signal{
		if(nb_emergency = max_emergency){
			set is_sature <- true;
			list_hosp <- (centre_urgence at_distance(rayon_emission)) where(!each.is_sature);
		}else if(amb_needed){
			list_hosp <- (centre_urgence at_distance(rayon_emission)) where(!each.amb_needed);
		}
	}
	// action d'envoi d'ambulance pour la recherche d'un patient
	
	
		action send_ambulance(centre_urgence chu){
		//write("je suis dans send_ambulance");
		if(!amb_to_other){
			loop pat over: list_accident{
				if(nb_amb_used < max_ambulance and nb_emergency < max_emergency){
					create ambulance number:1{
						set hosp <- chu;
						set location <- any_location_in(one_of(centre_urgence));//any_location_in(self);
						set target <- point(pat.location.x, pat.location.y);
						set dest <- point(self.location.x, self.location.y);
						//set dest <- any_location_in(one_of(centre_urgence at_distance(20)));
						//set dest  <- centre_urgence where(distance_to(self,each)>=50.0);
					}
					set nb_amb_used <- nb_amb_used + 1; 
				}else if(nb_amb_used >= max_ambulance and nb_emergency < max_emergency){
					do ask_ambulance;
				}
			}
		}else{
			loop pat over: chu.list_accident{
				if(nb_amb_used < max_ambulance){
					create ambulance number:1{
						set hosp <- chu;
						set location <- any_location_in(hosp);
						set target <- point(pat.location.x, pat.location.y);
						set dest <- point(chu.location.x, chu.location.y);
					}
					set nb_amb_used <- nb_amb_used + 1; 
				}
			}
		}

	}
	
	
	// action de demander une ambulance a un autre hopital
	action ask_ambulance{
		set amb_needed <- true;
		do send_signal;
	}
	// action de partage d'informations entre les hopitaux
	action share_info{
		
	}
	// aspect de l'agent
	aspect base {
		draw shape color: rgb('red');
	}
}

// species Ambulance
species ambulance parent:Position skills:[advanced_driving]{
	
	rgb color;
	float size;
	point target <- nil;
	float ambulance_speed <- rnd(100.0)+5.0;
	
	personne malad <- nil;
	centre_urgence hosp <- nil;
	point dest <- nil;
	
	reflex move when: target != nil{
		//target <- one_of(road);
		do goto target:target on:the_graph;
		//current_path <- compute_path(graph: the_graph, target: target );
		//do goto target:target on: the_graph;
		if(location = target){
			set malad.location <- any_location_in(hosp);
			set target <- dest;
		}
	}
	
	
	// aspect de l'agent
	aspect base {
		draw square(10) rotate:90 + heading color: #yellow;
	}
}
// species roads
species road{
	rgb color <- #gray;
	aspect base{
		draw shape +5 color: color;
	}
}

experiment urgence type: gui {
	/** Insert here the definition of the input and output of the model */
	output {
		//affichage de la ville et de ses habitants
		display Display_Ville type: opengl{
            species road aspect:base;
           // species vehicule aspect:base;
            species centre_urgence aspect:base; 
            species personne aspect:base;  
            species ambulance aspect:base;       
        }
        
       
		monitor "Nombre d'urgence" value: nb_urgence ;
		monitor "Nombre de survivant" value: nb_de_survivant ;
		monitor "Nombre de mort" value: nb_de_mort ;
				
        //affichage de la statistique
		display Series_Stat_Display{
            	chart "Statistique d'intervention" type: series {
                data "Nombre d'urgence" value: nb_cas_accidents color: #black;
                data "Nombre de Personnes sauvees" value: nb_de_survivant color: #blue;
                data "Nombre de deces" value: nb_de_mort color: #red;
                data "Pourcentage sauve" value: taux_sauvtage color: #green;
                data "Pourcentage deces" value: taux_non_sauve color: #yellow;
            }
        }
	}
}
