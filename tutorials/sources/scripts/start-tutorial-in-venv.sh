source tutorials_venv/bin/activate

# Open HTML version of tutorial in Firefox and run in the background
firefox book/index.html &

# Optionally, you can add a short delay to ensure the browser opens before JupyterLab starts
sleep 2

# Now, run JupyterLab
jupyter lab --ip=0.0.0.0 --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True' --browser=firefox --port-retries=100
