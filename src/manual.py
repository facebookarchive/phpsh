try:
    from pysqlite2 import dbapi2 as sqlite
except ImportError:
    from sqlite3 import dbapi2 as sqlite
import html2text
import logging
import os
import re

__author__ = "ccheever" # Charlie Cheever <charlie@facebook.com>
__date__ = "Mon Jun 26 03:04:55 PDT 2006"

php_manual_url = "http://us2.php.net/distributions/manual/php_manual_en.html.gz"
max_doc_size = 30000

_identifier_match = re.compile('<div id="([^"]*)')

def _get_documentation(url=php_manual_url):
    # todo: actually grab here if grabbing manually is annoying enough
    return open("php_manual_en.html")

def esc(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '""') + '"'

def _insert_documentation_into_db():
    """goes through the documentation file and pulls out the relevant code and
    then inserts stuff into the db.  run from phpsh src/ dir."""
    logging.basicConfig(level=logging.INFO)

    conn = sqlite.connect("php_manual.db")
    lines = _get_documentation()

    documentation = ""
    found_first = False
    name = None

    cursor = conn.cursor()
    cursor.execute("CREATE TABLE php_manual " +
        "(identifier VARCHAR(255) PRIMARY KEY, doc TEXT)")
    conn.commit()

    for line in lines:
        matches = _identifier_match.search(line)
        if matches:
            if found_first:
                if len(documentation) > max_doc_size:
                    documentation = documentation[:max_doc_size]
                    logging.warn("truncating documentation for %s" % name)
                cursor = conn.cursor()
                sql = \
                    "REPLACE INTO php_manual (identifier, doc) VALUES (%s, %s)" \
                    % (esc(name.lower()), esc(documentation))
                cursor.execute(sql)
                conn.commit()
            else:
                found_first = True
            documentation = ""

            name = matches.group(1)
            print name
            logging.debug("name=%s" % name)
        else:
            documentation += line
    conn.close()


def get_documentation_for_identifier(identifier, short=True):
    identifier = identifier.replace("_", "-").lower()
    manual_file = "php_manual.db"
    manual_path = os.path.join(os.getenv("HOME"), ".phpsh", manual_file)
    if not os.path.exists(manual_path):
        manual_path = os.path.join("/etc/phpsh", manual_file)
    conn = sqlite.connect(manual_path)
    cursor = conn.cursor()

    sql = "SELECT doc FROM php_manual " + \
        "WHERE identifier = %s OR identifier = %s LIMIT 1" % \
        (esc("function." + identifier), esc(identifier))
    cursor.execute(sql)

    try:
        rows = cursor.fetchall()
    except:
        logging.error("Query to get documentation from php manual failed")
        return "Could not query manual db"
    conn.close()

    if rows:
        ((doc,),) = rows
        if short:
            doc = doc[:doc.find("Example")]
        return html2text.html2text(doc).strip()
