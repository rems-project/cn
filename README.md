[![CN Proof](https://github.com/rems-project/cn/actions/workflows/ci-cn.yml/badge.svg)](https://github.com/rems-project/cn/actions/workflows/ci-cn.yml)
[![CN Spec Testing](https://github.com/rems-project/cn/actions/workflows/ci-cn-spec-testing.yml/badge.svg)](https://github.com/rems-project/cn/actions/workflows/ci-cn-spec-testing.yml)
[![CN-Coq](https://github.com/rems-project/cn/actions/workflows/ci-cn-coq.yml/badge.svg)](https://github.com/rems-project/cn/actions/workflows/ci-cn-coq.yml)



# CN

CN is a tool for verifying that C code is free of undefined behaviour and meets
user-written specifications of its ownership and functional correctness, and for translating those specifications into
C assertions that can be checked at runtime on concrete test cases.
It builds on the [Cerberus C semantics](https://github.com/rems-project/cerberus).

## Papers


<ul>

<li> <a name="fulminate-popl2025"></a> 
   <a href="http://www.cl.cam.ac.uk/users/pes20/cn-testing-popl2025.pdf">Fulminate: Testing CN Separation-Logic Specifications in C</a>.
 Rini Banerjee, Kayvan Memarian, Dhruv Makwana, Christopher Pulte, Neel Krishnaswami, and Peter Sewell.
 In POPL 2025.
[
<a href="http://dx.doi.org/10.1145/3704879">doi</a>&nbsp;| 
<a href="http://www.cl.cam.ac.uk/users/pes20/cn-testing-popl2025.pdf">pdf</a> 
]
</li>


<li>
<a name="2023-popl-cn"></a>
<a href="http://www.cl.cam.ac.uk/users/pes20/cn-draft.pdf">CN: Verifying systems C code with separation-logic refinement types</a>.
 Christopher Pulte, Dhruv&nbsp;C. Makwana, Thomas Sewell, Kayvan Memarian, Peter Sewell, and Neel Krishnaswami.
 In POPL 2023.
[
<a href="http://dx.doi.org/10.1145/3571194">doi</a>&nbsp;| 
<a href="https://www.cl.cam.ac.uk/~cp526/popl23.html">project page</a>&nbsp;| 
<a href="http://www.cl.cam.ac.uk/users/pes20/cn-draft.pdf">pdf</a>
]
</li>
</ul>

## Tutorial

See the [tutorial documentation](https://rems-project.github.io/cn-tutorial/).

## Installation

Below are the installation instructions for installing CN,
and its dependencies.


1. Install make, git, GMP library, pkg-config and either/both Z3 or CVC5.
   On an Ubuntu system this is done via
   ```
   sudo apt install build-essential libgmp-dev pkg-config z3
   ```
   Note: there is a [known bug with Z3 version
   4.8.13](https://github.com/rems-project/cerberus/issues/663) (the default on
   Ubuntu 22.04) so you may wish to install Z3 via opam later for a more
   up-to-date version. Z3 that is provided in the docker images is sufficiently up-to-date.

2. Install the opam package manager for OCaml:
   https://ocaml.org/docs/installing-ocaml#install-opam.
   On Ubuntu, `sudo apt install opam`.

3. Initialise opam with a recent version of OCaml (the CI builds with 4.14.1,
   CN developers use 5.2.0).
   ```
   opam init --yes --compiler=5.2.0
   ````

   Make sure you follow the instructions provided at the end of the output of `opam init` to complete the initialisation. Typically, on Unix, this is:

   ```
   eval $(opam env)
   ```

4. Clone the CN repo:
   ```
   git clone https://github.com/rems-project/cn.git
   ```

5. For CN end users, who don't want to tinker with CN locally:
   ```
   opam install --yes ./cn.opam # z3 for a more recent version
   ```

6. For CN developers:
   ```
   opam install --deps-only ./cn.opam ocamlformat.0.27.0 # one time
   make install # after any edits
   ```
   which installs CN (as both a library and an executable), and
   dependencies.

## Contributing

Please see our [contributing
guide](https://github.com/rems-project/cn/blob/main/doc/CONTRIBUTING.md)
for logistics and our [onboarding
guide](https://github.com/rems-project/cn/blob/main/doc/ONBOARDING.md)
for learning the code base.


## Funding

This software has received funding from the European Research Council
(ERC) under the European Union's Horizon 2020 research and innovation
programme from grant agreement no. 101002277 "TypeFoundry", and no. 789108,
ERC-AdG-2017 "ELVER"; from the UK Research and Innovation (UKRI) under the
UK government’s Horizon Europe funding guarantee for ERC-AdG-2022,
EP/Y035976/1 "SAFER"; from an EPSRC Doctoral Training studentship;
from Google; and from a Royal Society University Research Fellowship 
URF\R1\241195 (Pulte).

This material is based upon work supported by the Air Force Research Laboratory 
(AFRL) and Defense Advanced Research Projects Agencies (DARPA) under Contract 
No. FA8750-24-C-B044.  Any opinions, findings, and conclusions or 
recommendations expressed in this material are those of the author(s) and do 
not necessarily reflect the views of the AFRL and DARPA.