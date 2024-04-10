package com.luma.wadloader3.ddd3domain.config;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.ConstructorBinding;

@Value
@ConfigurationProperties(prefix = "application")
public class ApplicationProperties {

    String clientOriginUrl;

    @ConstructorBinding
    public ApplicationProperties(final String clientOriginUrl) {
        this.clientOriginUrl = clientOriginUrl;
    }

}
