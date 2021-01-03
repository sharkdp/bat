interface Display {
    String asText()
}

trait Entity {
    Integer id
}

class Product implements Entity, Display {
    public String name
    public Boolean available
    public Float price

    private String key
    protected String data

    /**
     * Creates a new product instance.
     * @param id Product ID.
     * @param name Product name.
     * @param available Product availability.
     * @param price Product price.
     * @param key Product key.
     * @param data Product internal data.
     */
    Product(id, name, available, price, key = "key", data = "internal") {
        this.id = id
        this.name = name
        this.available = available
        this.price = price
        this.key = key
        this.data = data
    }

    /**@
     * Returns product data as text.
     * @return Data string.
     */
    String asText() {
        return """ID [${id}] Name [${name}] Available [${available}] Price [${price}]"""
    }
}

/* Creates a new product instance */
def product = new Product(1, "T-Shirt", true, 15.00)

println(product.asText())

product.available = false
product.price = 0.0

// Check values
assert product.asText() == "ID [1] Name [T-Shirt] Available [false] Price [0.0]"

def factorial(Integer value) {
    if (value <= 1) {
        return 1
    } else {
        return value * factorial(value - 1)
    }
}

assert factorial(5) == 120

static String join(List<String> list, String separator) {
    String data = ""

    list.each { item ->
        data += item + separator
    }

    data = data.substring(0, data.length() - 1)

    return data
}

assert join(["g", "r", "o", "o", "v", "y"], " ") == "g r o o v y"
