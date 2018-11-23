export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

PS1='\w\$ '


alias webmToMp3='for f in *.webm; do ffmpeg -i "$f" -ab 320k "${f%.wav}.mp3"; done'
alias elint='yarn add --dev prettier eslint-config-prettier eslint-plugin-prettier esling babel-eslint eslint-plugin-react && eslint --init'
alias fixCod='autocmd BufWritePost *.js AsyncRun -post=checktime ./node_modules/.bin/eslint --fix %'
alias nis='npm install --save'
alias nisd='npm install --save-dev'
alias mpve='mpv --lua ~/.config/mpv/scripts/excerpt.lua --script-opts=osc-layout=bottombar'
alias pdf='(mupdf-gl &) && '



# MacPorts Installer addition on 2017-11-07_at_16:02:15: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

# Add Visual Studio Code (code)
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

## postgress environment variables

export DATABASE_USER=postgres
export DATABASE_PASSWORD=secret 
export DATABASE_HOST=localhost 
export DATABASE_PORT=5432 
export DATABASE_DB=demo

# OPAM configuration
. /Users/leo/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
