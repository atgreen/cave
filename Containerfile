## Cave — self-hosted code forge
##
## Build:  podman build -t cave .
## Run:    podman run -d --name cave -p 8080:8080 -p 2222:22 \
##           -v cave-data:/var/lib/cave cave:latest
##
## Requires a PostgreSQL container running separately.

FROM fedora:42 AS builder

RUN dnf install -y sbcl make git gcc openssl-devel && dnf clean all

WORKDIR /build
COPY ocicl.csv cave.asd Makefile ./
COPY src/ src/
COPY static/ static/

# Install ocicl and deps
RUN curl -sSL https://www.ocicl.dev/install.sh | bash && \
    export PATH="$HOME/.local/bin:$PATH" && \
    ocicl setup > /tmp/setup.lisp && \
    sbcl --non-interactive --load /tmp/setup.lisp --eval '(asdf:load-system :ocicl)' --eval '(ocicl:install)' && \
    make

## --- Runtime image ---

FROM fedora:42

RUN dnf install -y openssh-server git && dnf clean all && \
    # Create cave user
    useradd -m -s /bin/bash cave && \
    # Configure sshd
    mkdir -p /etc/ssh && \
    ssh-keygen -A && \
    # Harden sshd config
    sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    sed -i 's/#PermitRootLogin.*/PermitRootLogin no/' /etc/ssh/sshd_config && \
    # Create data dirs
    mkdir -p /var/lib/cave/repos /var/lib/cave/tmp && \
    chown -R cave:cave /var/lib/cave && \
    # Create SSH dir for cave user
    mkdir -p /home/cave/.ssh && \
    chmod 700 /home/cave/.ssh && \
    chown cave:cave /home/cave/.ssh

COPY --from=builder /build/cave /usr/bin/cave
COPY static/ /opt/cave/static/
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh /usr/bin/cave

EXPOSE 8080 22

VOLUME /var/lib/cave

ENTRYPOINT ["/entrypoint.sh"]
