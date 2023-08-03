getwd()
source("trendanalyse_function_2018.R")
source("trendanalyse_2018_ohne_einflussreiche_beob.R")


# Führe die Trendanalyse mit dem 1. Datensatz durch
perform_trendanalyse(data_dir =  "C:/Users/Marvin/LRZ Sync+Share/Statistsiches Praktikum 2122/Trendschätzung Code/Input2/",
                     #plot_dir = "C://Users/Banu/Desktop/Lorenz_BA/R-Code_2018/plots-yearly/",
                     #result_name = "C://Users/Banu/Desktop/Lorenz_BA/R-Code_2018/ergebnis_tabelle_")
                     plot_dir =  "C:/Users/Marvin/LRZ Sync+Share/Statistsiches Praktikum 2122/Trendschätzung Code/Output2/",
                     result_name =  "C:/Users/Marvin/LRZ Sync+Share/Statistsiches Praktikum 2122/Trendschätzung Code/Output2/")

# nutze die logarithmierten Daten
perform_trendanalyse(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/bearbeitete_Daten_logarithmiert/",
                     plot_dir = "../Ergebnisse_2018/plots_yearly_log/",
                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_log_",
                     log_trans = TRUE)

# schließe einflussreiche Beobachtungen aus
perform_trendanalyse_ohne_einfl_beob(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/bearbeitete_Daten/",
                                     plot_dir = "../Ergebnisse_2018/plots_yearly_ohne_einfl_beob/",
                                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_ohne_einfl_beob_",
                                     res_tab_file = "../Ergebnisse_2018/Ergebnis_Tabelle_2018-11-15.csv")

perform_trendanalyse_ohne_einfl_beob(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/bearbeitete_Daten_logarithmiert/",
                                     plot_dir = "../Ergebnisse_2018/plots_yearly_log_ohne_einfl_beob/",
                                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_log_ohne_einfl_beob_",
                                     res_tab_file = "../Ergebnisse_2018/Ergebnis_Tabelle_log_2018-11-15.csv",
                                     log_trans = TRUE)



# Führe die Trendanalyse mit dem 2. Datensatz durch
perform_trendanalyse(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/Datensatz_2/bearbeitete_Daten/",
                     plot_dir = "../Ergebnisse_2018/plots_yearly/Datensatz_2/",
                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_")

# nutze die logarithmierten Daten
perform_trendanalyse(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/Datensatz_2/bearbeitete_Daten_logarithmiert/",
                     plot_dir = "../Ergebnisse_2018/plots_yearly_log/Datensatz_2/",
                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_log_",
                     log_trans = TRUE)

# schließe die einflussreichen Beobachtungen aus
perform_trendanalyse_ohne_einfl_beob(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/Datensatz_2/bearbeitete_Daten/",
                                     plot_dir = "../Ergebnisse_2018/plots_yearly_ohne_einfl_beob/Datensatz_2/",
                                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_ohne_einfl_beob_",
                                     res_tab_file = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_2018-11-15.csv")
# an sich nicht benötigt
perform_trendanalyse_ohne_einfl_beob(data_dir = "../Daten_2018/Datensatz_bis_2018_10_31/Datensatz_2/bearbeitete_Daten_logarithmiert/",
                                     plot_dir = "../Ergebnisse_2018/plots_yearly_log_ohne_einfl_beob/Datensatz_2/",
                                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_log_ohne_einfl_beob",
                                     res_tab_file = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_2_log_2018-11-15.csv",
                                     log_trans = TRUE)

# Führe die Trendanalyse mit dem 3. Datensatz durch
perform_trendanalyse(data_dir = "../Daten_2018/Datensatz_bis_2018_11_13/bearbeitete_Daten/",
                     plot_dir = "../Ergebnisse_2018/plots_yearly/Datensatz_3/",
                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_3_")

perform_trendanalyse(data_dir = "../Daten_2018/Datensatz_bis_2018_11_13/bearbeitete_Daten_logarithmiert/",
                     plot_dir = "../Ergebnisse_2018/plots_yearly_log/Datensatz_3/",
                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_3_log_",
                     log_trans = TRUE)

# schließe die einflussreichen Beobachtungen aus
perform_trendanalyse_ohne_einfl_beob(data_dir = "../Daten_2018/Datensatz_bis_2018_11_13/bearbeitete_Daten/",
                                     plot_dir = "../Ergebnisse_2018/plots_yearly_ohne_einfl_beob/Datensatz_3/",
                                     result_name = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_3_ohne_einfl_beob_",
                                     res_tab_file = "../Ergebnisse_2018/Ergebnis_Tabelle_dataset_3_2018-11-15.csv")
