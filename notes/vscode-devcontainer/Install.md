# Plutus Visual Studio Code DevContioner

This is a docker container configured for use with Visual Studio Code
that includes the Haskell Language Server to provide features to make
it easier to develop Plutus code.


## Setting up Visual Studio Code and the Plutus DevContainer

### Install Visual Studio Code

Follow these instructions for [Setting up Visual Studio Code](https://code.visualstudio.com/docs/setup/setup-overview)


### Install Docker

The DevContainer is a docker container so you will need to have
docker installed on your system.  You can install docker from
[here](https://www.docker.com/get-started).  If you are not
sure which of the install options to choose you should
most likely choose the Docker Desktop.


### Install the Remote Containers Extension

Got to the [Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
extension page ad click on the Install link.  Depending on your os/browser
you will be asked something like "Do you want to allow this page to open
"Visual Studio Code.app"?".
Choosing "Allow" or "Open Visual Studio Code.app" should cause Visual Studio
Code to open and the extension to be installed.


### Openning the plutus-contract-template in a Plutus DevContainer

To try out the DevContainer with the plutus-contract-template:

* Download the hkm/devcontainer branch of plutus:
    git clone https://github.com/input-output-hk/plutus.git
    cd plutus
    git checkout hkm/devcontainer
* In Visual Studion Code press F1 to bring up the Command Palette.
* Select the `Remote-Containers: Open Folder in Container...` command.
* Select the plutus-contract-template directory.
* At this point Visual Studio Code will downlaod and build the DevContainer
  and it may take some time.
* Select Visual Studio Code's Explore pane (click on the icon in the top
  left to bring this up)
* In the Explorer you should see "PLUTUS-CONTRACT-TEMPLATE [DEV CONTAINER: PLUTUS]",
  under that you should be able to open files in
    src /
* If the DevContainer is working correctly these files hould have syntax
  highlighting and tooltips whith type information.

### Openning the plutus-uses-cases in a Plutus DevContainer

To try out the DevContainer with the plutus-uses-cases
instead of the plutus-contract-template:

* Add a plutus/plutus-use-cases/hie.yaml file containg:
  ```
  cradle:
    cabal:
    - path: ./src
      component: "lib:plutus-use-cases"
    - path: ./test
      component: "plutus-use-cases:plutus-use-cases-test"
  ```
