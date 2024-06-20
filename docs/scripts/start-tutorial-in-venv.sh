source tutorial_venv/bin/activate

echo -e "\nOpening HTML and JupyterLab!\n"
firefox _build/html/index.html &

# Now, run JupyterLab
jupyter lab --ip=0.0.0.0  --notebook-dir=_build/jupyter_execute/tutorial/ --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True' --browser=firefox --port-retries=100
