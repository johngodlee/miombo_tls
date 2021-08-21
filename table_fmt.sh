#!/usr/bin/env sh

sed '12,13s/1//' out/indval.tex |\
	sed '16,17s/2//' |\
	sed '20,21s/3//' |\
	sed '24,25s/4//' |\
	sed '11s/1/{\\multirow{3}{*}{1}}/' |\
	sed '15s/2/{\\multirow{3}{*}{2}}/' |\
	sed '19s/3/{\\multirow{3}{*}{3}}/' |\
	sed '23s/4/{\\multirow{3}{*}{4}}/' |\
	sed '9s/{Indicator value}/{\\thead{Indicator\\\\value}}/' > out/indval_fmt.tex

sed '9s/{Stem density}/\\thead{Stem density\\\\(Stems ha\\textsuperscript{-1})}/' out/clust_summ.tex |\
	sed '9s/{AGB}/\\thead{AGB\\\\(t ha\\textsuperscript{-1})}/' > out/clust_summ_fmt.tex

sed '9s/{CV basal area}/{\\thead{Basal area\\\\CV}}/' out/canopy_rough_dredge_best.tex |\
	sed '9s/{CV Voronoi}/{\\thead{Voronoi\\\\CV}}/' |\
	sed	'9s/{Tree density}/{\\thead{Tree\\\\density}}/' |\
	sed	'9s/{Winkelmass}/{\\thead{Uniform\\\\angle index}}/' |\
	sed '7i\\\setlength{\\tabcolsep}{4pt}' > out/canopy_rough_dredge_best_fmt.tex

sed '9s/{CV basal area}/{\\thead{Basal area\\\\CV}}/' out/height_profile_dredge_best.tex > out/height_profile_dredge_best_fmt.tex


sed '3i\\\setlength{\\tabcolsep}{4pt}' out/bivar_lm_summ.tex |\
	sed 's/{Winkelmass}/{Uniform angle index}/g' > out/bivar_lm_summ_fmt.tex
