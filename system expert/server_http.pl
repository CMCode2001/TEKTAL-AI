:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module(moteur_inference).  % Importer le moteur d'inférence
:- use_module(knowledge_base). 

% Démarrer le serveur sur le port 8000
server(8000) :- http_server(http_dispatch, [port(8000)]).

% Route pour le diagnostic
:- http_handler(root(lister_metiers), handle_metier, []).
:- http_handler(root(lister_domaines), handle_domaine, []).

handle_domaine(Request) :-
    findall(D, domaine(D), Doms),  % Récupérer toutes les maladies
    reply_json_dict(_{domaines: Doms}).    % Renvoyer le diagnostic en JSON

handle_metier(_Request) :-
    findall(_{metier: Metier, domaine: Domaine}, metier(Metier, Domaine), Metiers),
    reply_json_dict(_{metiers: Metiers}).


% handle_metier(Request) :-
%     findall(M, metier(M,_), Metiers),  % Récupérer toutes les maladies
%     reply_json_dict(_{metiers: Metiers}).    % Renvoyer le diagnostic en JSON

% Lancer le serveur automatiquement au démarrage
:- initialization(server(8000)).