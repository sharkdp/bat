required_packages:
  pkg.installed:
    - pkgs:
      - git
      - perl
      - fortune

cowsay_source:
  git.latest:
    - name: https://github.com/jasonm23/cowsay.git
    - target: /root/cowsay

run_installer:
  cmd.run:
    - name: ./install.sh /usr/local
    - cwd: /root/cowsay
    - onchanges:
      - git: cowsay_source

{% set cowfiles = salt.cmd.run('cowsay -l').split('\n')[1:] %}
{% set ascii_arts = cowfiles | join(' ') %}

{% for ascii_art in ascii_arts.split(' ') %}
run_cowsay_{{ ascii_art }}: # name must be unique
  cmd.run:
    {% if ascii_art is in ['head-in', 'sodomized', 'telebears'] %}
    - name: echo cowsay -f {{ ascii_art }} should not be used
    {% else %}
    - name: fortune | cowsay -f {{ ascii_art }}
    {% endif %}
{% endfor %}

echo_pillar_demo_1:
  cmd.run:
    - name: "echo {{ pillar.demo_text | default('pillar not defined') }}"

echo_pillar_demo_2:
  cmd.run:
    - name: "echo {{ pillar.demo.text | default('pillar not defined') }}"

# Comment
{% set rand = salt['random.get_str'](20) %}
{% set IP_Address = pillar['IP_Address'] %}

wait:
  cmd.run:
    - name: sleep 210  # another comment

create_roster_file:
  file.managed:
    - name: /tmp/salt-roster-{{ rand }}
    - contents:
      - 'switch:'
      - '  host: {{ IP_Address }}'
      - "  user: test"
      - "  passwd: {{ passwd }}"

