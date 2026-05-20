class foo ($bar) {
  @@notify { 'foo': }
}
@@file { "somedir/${name}_${munin_port_real}":
  ensure => present,
  content => template("munin/defaultclient.erb"),
}
# Collect all exported files
File <<||>>

# Compile the munin.conf with a local header
concatenated_file { "/etc/munin/munin.conf":
    dir => somedir,
    header => "/etc/munin/munin.conf.header",
}
hosting_vserver_configuration {
    "erics":
        domain => "orange.co",
        type => "friend",
        context => 13,
        ip => "255.255.255.254", prefix => 27,
        admin_user => "erict", admin_user_name => "hello, its me",
        admin_user_email => "erict@orange.co",
        customer => "hello? is it me?",
        admin_password => file("/etc/puppet/secrets/hosting/erict_passwd"),
}
class davids_black_co_at {
    ## Create users for my parents and my grandmother
    hosting::user {
        rztt: realname => "some other rztt",
            uid => 2001, admin => true;
        same: realname => "could be same",
            uid => 2002;
        imapersontoodamnit: realname => "some one else",
            uid => 2003;
    }

    # Install git.black.co.at
    include git::daemon
    include git::web
    git::web::export { [manifests, "puppet-trunk"]: }

    # Provision an additional mysql database on the database server
    hosting::database { "fogbugz": type => mysql }
    # Create another VirtualHost
    apache2::site { "local-fogbugz":
        source => "puppet://$servername/files/hosting/erict/sites/local-fogbugz"
    }
}
node backuppc {
        # only use the smarthost
        $mta = ssmtp
        # this is a vserver on this host, so register correctly in nagios
        $nagios_parent = "orange.co"
        # I'm sharing an IP here, so those things have to have their own ports
        $apache2_port = 8080
        $munin_port = 5008
        $munin_stats_port = 8667

        # default configuration
        include dbp

        # configure the backuppc server
        include backuppc::server
}
