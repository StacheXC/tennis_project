Steps to reproduce analysis (Several intermediate files were too large to push to github):

1\. Run data/compile_data.R to put together the entire dataset (verify with companion illustration script)

2\. Run reward_surface/fit_reward_surface.R (\~1 day to fit) (verify with illustration script)

3\. Run execution_error/body/fit_execution_error.R (\~3 days) (verify with illustration script)

4\. Run optimums/get_optimums.R (\~1 day) (verify with illustration script)

5\. Run results/bias_plots.R and results/regression.R

Steps 2 and 3 can be done simultaneously. Step 4 cannot be done until both the reward surface and the execution error models are fit.
