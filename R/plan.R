the_plan <-
  drake_plan(

    xl_files = {
      fnames <- list.files(file_in("data/TB Portals Published data_20201021/"), full.names = T)
      o <- map(fnames, .f = function(n) read_csv(file = n))
      names(o) <- basename(fnames)
      return(o)
    },

    initial_df = generate_initial_df(xl_files),

    supplementary_data_initial_df_tby = generate_initial_df_tableby(initial_df),

    initial_df_preprocess = preprocess_initial_df(initial_df),

    initial_df_preprocess_tby = generate_initial_df_preprocess_tableby(initial_df_preprocess),

    case_characteristics_tby = generate_case_characteristics_tableby(initial_df, xl_files),

    mlr3_tasks = setup_mlr3_tasks(initial_df_preprocess),

    mlr3_pipeops = setup_mlr3_pipeops(),

    corr_plot = create_corrplot(mlr3_tasks, mlr3_pipeops),

    binary_bm = create_binary_bm(mlr3_pipeops, mlr3_tasks),

    survival_bm = create_survival_bm(mlr3_pipeops, mlr3_tasks),

    logreg_filt_finalfit = create_logreg_filtered_finalfit(mlr3_pipeops, mlr3_tasks),

    coxph_filt_finalfit = create_coxph_filtered_finalfit(mlr3_pipeops, mlr3_tasks),

    kaplan_meier_plots = create_kaplan_meier_plots(mlr3_tasks)
)
